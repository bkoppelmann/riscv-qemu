/*
 * RISC-V emulation for qemu: main translation routines.
 *
 * Author: Sagar Karandikar, sagark@eecs.berkeley.edu
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "disas/disas.h"
#include "tcg-op.h"
#include "exec/cpu_ldst.h"


#include "exec/helper-proto.h"
#include "exec/helper-gen.h"

#include "exec/log.h"


#define RISCV_DEBUG_DISAS 0

/* global register indices */
static TCGv_ptr cpu_env;
static TCGv cpu_gpr[32], cpu_pc;
static TCGv_i64 cpu_fpr[32]; /* assume F and D extensions */
static TCGv load_res;
#ifdef CONFIG_USER_ONLY
static TCGv_i32 cpu_amoinsn;
#endif

#include "exec/gen-icount.h"

#define RISCV_DECODE_HEADER
#include "insn-decoder.h"
#undef RISCV_DECODE_HEADER

typedef struct DisasContext {
    struct TranslationBlock *tb;
    target_ulong pc;
    target_ulong next_pc;
    DisasFormats fmt;
    uint32_t opcode;
    int singlestep_enabled;
    int mem_idx;
    int bstate;
} DisasContext;

static inline void kill_unknown(DisasContext *ctx, int excp);

enum {
    BS_NONE     = 0, /* When seen outside of translation while loop, indicates
                     need to exit tb due to end of page. */
    BS_STOP     = 1, /* Need to exit tb for syscall, sret, etc. */
    BS_BRANCH   = 2, /* Need to exit tb for branch, jal, etc. */
};


static const char * const regnames[] = {
  "zero", "ra  ", "sp  ", "gp  ", "tp  ", "t0  ",  "t1  ",  "t2  ",
  "s0  ", "s1  ", "a0  ", "a1  ", "a2  ", "a3  ",  "a4  ",  "a5  ",
  "a6  ", "a7  ", "s2  ", "s3  ", "s4  ", "s5  ",  "s6  ",  "s7  ",
  "s8  ", "s9  ", "s10 ", "s11 ", "t3  ", "t4  ",  "t5  ",  "t6  "
};

static const char * const fpr_regnames[] = {
  "ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
  "fs0", "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
  "fa6", "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
  "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11"
};

static inline void generate_exception(DisasContext *ctx, int excp)
{
    tcg_gen_movi_tl(cpu_pc, ctx->pc);
    TCGv_i32 helper_tmp = tcg_const_i32(excp);
    gen_helper_raise_exception(cpu_env, helper_tmp);
    tcg_temp_free_i32(helper_tmp);
}

static inline void generate_exception_mbadaddr(DisasContext *ctx, int excp)
{
    tcg_gen_movi_tl(cpu_pc, ctx->pc);
    TCGv_i32 helper_tmp = tcg_const_i32(excp);
    gen_helper_raise_exception_mbadaddr(cpu_env, helper_tmp, cpu_pc);
    tcg_temp_free_i32(helper_tmp);
}
/* unknown instruction */
static inline void kill_unknown(DisasContext *ctx, int excp)
{
    generate_exception(ctx, excp);
    ctx->bstate = BS_STOP;
}

static inline bool use_goto_tb(DisasContext *ctx, target_ulong dest)
{
    if (unlikely(ctx->singlestep_enabled)) {
        return false;
    }

#ifndef CONFIG_USER_ONLY
    return (ctx->tb->pc & TARGET_PAGE_MASK) == (dest & TARGET_PAGE_MASK);
#else
    return true;
#endif
}

static inline void gen_goto_tb(DisasContext *ctx, int n, target_ulong dest)
{
    if (use_goto_tb(ctx, dest)) {
        /* chaining is only allowed when the jump is to the same page */
        tcg_gen_goto_tb(n);
        tcg_gen_movi_tl(cpu_pc, dest);
        tcg_gen_exit_tb((uintptr_t)ctx->tb + n);
    } else {
        tcg_gen_movi_tl(cpu_pc, dest);
        if (ctx->singlestep_enabled) {
            gen_helper_raise_exception_debug(cpu_env);
        }
        tcg_gen_exit_tb(0);
    }
}

/* Wrapper for getting reg values - need to check of reg is zero since
 * cpu_gpr[0] is not actually allocated
 */
static inline void gen_get_gpr(TCGv t, int reg_num)
{
    if (reg_num == 0) {
        tcg_gen_movi_tl(t, 0);
    } else {
        tcg_gen_mov_tl(t, cpu_gpr[reg_num]);
    }
}

/* Wrapper for setting reg values - need to check of reg is zero since
 * cpu_gpr[0] is not actually allocated. this is more for safety purposes,
 * since we usually avoid calling the OP_TYPE_gen function if we see a write to
 * $zero
 */
static inline void gen_set_gpr(int reg_num_dst, TCGv t)
{
    if (reg_num_dst != 0) {
        tcg_gen_mov_tl(cpu_gpr[reg_num_dst], t);
    }
}

static void gen_cond_branch(CPURISCVState *env, DisasContext *ctx, TCGCond cond,
                            int rs1, int rs2, target_ulong bimm)
{
    TCGLabel *l = gen_new_label();
    TCGv source1, source2;
    source1 = tcg_temp_new();
    source2 = tcg_temp_new();
    gen_get_gpr(source1, rs1);
    gen_get_gpr(source2, rs2);

    tcg_gen_brcond_tl(cond, source1, source2, l);

    gen_goto_tb(ctx, 1, ctx->next_pc);
    gen_set_label(l); /* branch taken */
    if (!riscv_feature(env, RISCV_FEATURE_RVC) && ((ctx->pc + bimm) & 0x3)) {
        /* misaligned */
        generate_exception_mbadaddr(ctx, RISCV_EXCP_INST_ADDR_MIS);
        tcg_gen_exit_tb(0);
    } else {
        gen_goto_tb(ctx, 0, ctx->pc + bimm);
    }
    tcg_temp_free(source1);
    tcg_temp_free(source2);
    ctx->bstate = BS_BRANCH;

}

static void gen_load(DisasContext *ctx, TCGMemOp memop, int rd, int rs1,
                     target_long imm)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    gen_get_gpr(t0, rs1);
    tcg_gen_addi_tl(t0, t0, imm);

    tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, memop);

    gen_set_gpr(rd, t1);
    tcg_temp_free(t0);
    tcg_temp_free(t1);
}

static void gen_store(DisasContext *ctx, TCGMemOp memop, int rs1, int rs2,
                      target_long imm)
{
    TCGv t0 = tcg_temp_new();
    TCGv dat = tcg_temp_new();
    gen_get_gpr(t0, rs1);
    tcg_gen_addi_tl(t0, t0, imm);
    gen_get_gpr(dat, rs2);

    tcg_gen_qemu_st_tl(dat, t0, ctx->mem_idx, memop);

    tcg_temp_free(t0);
    tcg_temp_free(dat);
}


static void gen_lui(CPURISCVState *env, DisasContext *ctx)
{
    if (ctx->fmt.field_rd == 0) {
            return; /* NOP */
    }
    tcg_gen_movi_tl(cpu_gpr[ctx->fmt.field_rd], ctx->fmt.immediate_U);
}

static void gen_auipc(CPURISCVState *env, DisasContext *ctx)
{
    if (ctx->fmt.field_rd == 0) {
            return; /* NOP */
    }
    tcg_gen_movi_tl(cpu_gpr[ctx->fmt.field_rd], ctx->fmt.immediate_U + ctx->pc);
}


static void gen_jal(CPURISCVState *env, DisasContext *ctx)
{
    target_ulong next_pc;

    /* check misaligned: */
    next_pc = ctx->pc + ctx->fmt.immediate_J;
    if (!riscv_feature(env, RISCV_FEATURE_RVC)) {
        if ((next_pc & 0x3) != 0) {
            generate_exception_mbadaddr(ctx, RISCV_EXCP_INST_ADDR_MIS);
        }
    }
    if (ctx->fmt.field_rd != 0) {
        tcg_gen_movi_tl(cpu_gpr[ctx->fmt.field_rd], ctx->next_pc);
    }

    gen_goto_tb(ctx, 0, ctx->pc + ctx->fmt.immediate_J); /* must use this for safety */
    ctx->bstate = BS_BRANCH;

}

static void gen_jalr(CPURISCVState *env, DisasContext *ctx)
{
    /* no chaining with JALR */
    TCGLabel *misaligned = gen_new_label();
    TCGv t0;
    t0 = tcg_temp_new();

    gen_get_gpr(cpu_pc, ctx->fmt.field_rs1);
    tcg_gen_addi_tl(cpu_pc, cpu_pc, ctx->fmt.immediate_I);
    tcg_gen_andi_tl(cpu_pc, cpu_pc, (target_ulong)-2);

    if (!riscv_feature(env, RISCV_FEATURE_RVC)) {
        tcg_gen_andi_tl(t0, cpu_pc, 0x2);
        tcg_gen_brcondi_tl(TCG_COND_NE, t0, 0x0, misaligned);
    }

    if (ctx->fmt.field_rd != 0) {
        tcg_gen_movi_tl(cpu_gpr[ctx->fmt.field_rd], ctx->next_pc);
    }
    tcg_gen_exit_tb(0);

    gen_set_label(misaligned);
    generate_exception_mbadaddr(ctx, RISCV_EXCP_INST_ADDR_MIS);
    tcg_gen_exit_tb(0);
    ctx->bstate = BS_BRANCH;

    tcg_temp_free(t0);
}

static void gen_beq(CPURISCVState *env, DisasContext *ctx)
{
    gen_cond_branch(env, ctx, TCG_COND_EQ, ctx->fmt.field_rs1,
                    ctx->fmt.field_rs2, ctx->fmt.immediate_B);
}

static void gen_bne(CPURISCVState *env, DisasContext *ctx)
{
    gen_cond_branch(env, ctx, TCG_COND_NE, ctx->fmt.field_rs1,
                    ctx->fmt.field_rs2, ctx->fmt.immediate_B);
}

static void gen_blt(CPURISCVState *env, DisasContext *ctx)
{
    gen_cond_branch(env, ctx, TCG_COND_LT, ctx->fmt.field_rs1,
                    ctx->fmt.field_rs2, ctx->fmt.immediate_B);
}

static void gen_bge(CPURISCVState *env, DisasContext *ctx)
{
    gen_cond_branch(env, ctx, TCG_COND_GE, ctx->fmt.field_rs1,
                    ctx->fmt.field_rs2, ctx->fmt.immediate_B);
}

static void gen_bltu(CPURISCVState *env, DisasContext *ctx)
{
    gen_cond_branch(env, ctx, TCG_COND_LTU, ctx->fmt.field_rs1,
                    ctx->fmt.field_rs2, ctx->fmt.immediate_B);
}

static void gen_bgeu(CPURISCVState *env, DisasContext *ctx)
{
    gen_cond_branch(env, ctx, TCG_COND_GEU, ctx->fmt.field_rs1,
                    ctx->fmt.field_rs2, ctx->fmt.immediate_B);
}

static void gen_lb(CPURISCVState *env, DisasContext *ctx)
{

    gen_load(ctx, MO_SB, ctx->fmt.field_rd, ctx->fmt.field_rs1,
             ctx->fmt.immediate_I);
}

static void gen_lh(CPURISCVState *env, DisasContext *ctx)
{

    gen_load(ctx, MO_LESW, ctx->fmt.field_rd, ctx->fmt.field_rs1,
             ctx->fmt.immediate_I);
}

static void gen_lw(CPURISCVState *env, DisasContext *ctx)
{

    gen_load(ctx, MO_LESL, ctx->fmt.field_rd, ctx->fmt.field_rs1,
             ctx->fmt.immediate_I);
}

static void gen_lbu(CPURISCVState *env, DisasContext *ctx)
{

    gen_load(ctx, MO_UB, ctx->fmt.field_rd, ctx->fmt.field_rs1,
             ctx->fmt.immediate_I);
}

static void gen_lhu(CPURISCVState *env, DisasContext *ctx)
{

    gen_load(ctx, MO_LEUW, ctx->fmt.field_rd, ctx->fmt.field_rs1,
             ctx->fmt.immediate_I);
}

static void gen_sb(CPURISCVState *env, DisasContext *ctx)
{

    gen_store(ctx, MO_SB, ctx->fmt.field_rs1, ctx->fmt.field_rs2,
              ctx->fmt.immediate_S);
}

static void gen_sh(CPURISCVState *env, DisasContext *ctx)
{

    gen_store(ctx, MO_LESW, ctx->fmt.field_rs1, ctx->fmt.field_rs2,
              ctx->fmt.immediate_S);
}

static void gen_sw(CPURISCVState *env, DisasContext *ctx)
{

    gen_store(ctx, MO_LESL, ctx->fmt.field_rs1, ctx->fmt.field_rs2,
              ctx->fmt.immediate_S);
}

#define I_PRELUDE\
    TCGv source1;     \
    source1 = tcg_temp_new();\
    gen_get_gpr(source1, ctx->fmt.field_rs1);\

#define I_CONCL \
    gen_set_gpr(ctx->fmt.field_rd, source1);\
    tcg_temp_free(source1);                 \

#define ARITHI(opname, func)                                   \
static void gen_##opname(CPURISCVState *env, DisasContext *ctx)\
{                                                              \
    I_PRELUDE                                                  \
    func(source1, source1, ctx->fmt.immediate_I);              \
    I_CONCL;\
}

ARITHI(addi, tcg_gen_addi_tl)
ARITHI(xori, tcg_gen_xori_tl)
ARITHI(ori, tcg_gen_ori_tl)
ARITHI(andi, tcg_gen_andi_tl)
ARITHI(slli, tcg_gen_shli_tl)
ARITHI(srli, tcg_gen_shri_tl)
ARITHI(srai, tcg_gen_sari_tl)
#undef ARITHI

#define ARITHI_SLT(opname, func, cond) \
static void gen_##opname(CPURISCVState *env, DisasContext *ctx)\
{                                                              \
    I_PRELUDE                                                  \
    func(cond, source1, source1, ctx->fmt.immediate_I);        \
    I_CONCL;\
}

ARITHI_SLT(slti, tcg_gen_setcondi_tl, TCG_COND_LT)
ARITHI_SLT(sltiu, tcg_gen_setcondi_tl, TCG_COND_LTU)
#undef ARITHI_SLT

#define R_PRELUDE\
    TCGv source1, source2;   \
    source1 = tcg_temp_new();\
    source2 = tcg_temp_new();\
    gen_get_gpr(source1, ctx->fmt.field_rs1);\
    gen_get_gpr(source2, ctx->fmt.field_rs2);\

#define R_CONCL \
    gen_set_gpr(ctx->fmt.field_rd, source1);\
    tcg_temp_free(source1);                 \
    tcg_temp_free(source2);                 \

#define ARITH(opname, func)                                   \
static void gen_##opname(CPURISCVState *env, DisasContext *ctx)\
{                                                              \
    R_PRELUDE                                                  \
    func(source1, source1, source2);                           \
    R_CONCL                                                    \
}

ARITH(add, tcg_gen_add_tl)
ARITH(sub, tcg_gen_sub_tl)
ARITH(xor, tcg_gen_xor_tl)
ARITH(or, tcg_gen_or_tl)
ARITH(and, tcg_gen_and_tl)
#undef ARITH

static void gen_sll(CPURISCVState *env, DisasContext *ctx)
{
    R_PRELUDE
    tcg_gen_andi_tl(source2, source2, TARGET_LONG_BITS - 1);
    tcg_gen_shl_tl(source1, source1, source2);
    R_CONCL
}

static void gen_srl(CPURISCVState *env, DisasContext *ctx)
{
    R_PRELUDE
    tcg_gen_andi_tl(source2, source2, TARGET_LONG_BITS - 1);
    tcg_gen_shr_tl(source1, source1, source2);
    R_CONCL
}

static void gen_sra(CPURISCVState *env, DisasContext *ctx)
{
    R_PRELUDE
    tcg_gen_andi_tl(source2, source2, TARGET_LONG_BITS - 1);
    tcg_gen_sar_tl(source1, source1, source2);
    R_CONCL
}

static void gen_slt(CPURISCVState *env, DisasContext *ctx)
{
    R_PRELUDE
    tcg_gen_setcond_tl(TCG_COND_LT, source1, source1, source2);
    R_CONCL
}

static void gen_sltu(CPURISCVState *env, DisasContext *ctx)
{
    R_PRELUDE
    tcg_gen_setcond_tl(TCG_COND_LTU, source1, source1, source2);
    R_CONCL
}
static void gen_fence(CPURISCVState *env, DisasContext *ctx)
{
    /* NOP */
}

static void gen_fence_i(CPURISCVState *env, DisasContext *ctx)
{
    gen_helper_fence_i(cpu_env);
    tcg_gen_movi_tl(cpu_pc, ctx->next_pc);
    tcg_gen_exit_tb(0); /* no chaining */
    ctx->bstate = BS_BRANCH;
}

static void gen_ecall(CPURISCVState *env, DisasContext *ctx)
{
    /* always generates U-level ECALL, fixed in do_interrupt handler */
    generate_exception(ctx, RISCV_EXCP_U_ECALL);
    tcg_gen_exit_tb(0); /* no chaining */
    ctx->bstate = BS_BRANCH;
}

static void gen_ebreak(CPURISCVState *env, DisasContext *ctx)
{
    generate_exception(ctx, RISCV_EXCP_BREAKPOINT);
    tcg_gen_exit_tb(0); /* no chaining */
    ctx->bstate = BS_BRANCH;
}

static void gen_csrrw(CPURISCVState *env, DisasContext *ctx)
{
}

static void gen_csrrs(CPURISCVState *env, DisasContext *ctx)
{
}

static void gen_csrrc(CPURISCVState *env, DisasContext *ctx)
{
}

static void gen_csrrwi(CPURISCVState *env, DisasContext *ctx)
{
}

static void gen_csrrsi(CPURISCVState *env, DisasContext *ctx)
{
}

static void gen_csrrci(CPURISCVState *env, DisasContext *ctx)
{
}

#include "insn-decoder.h"

void gen_intermediate_code(CPURISCVState *env, TranslationBlock *tb)
{
    RISCVCPU *cpu = riscv_env_get_cpu(env);
    CPUState *cs = CPU(cpu);
    DisasContext ctx;
    target_ulong pc_start;
    target_ulong next_page_start;
    int num_insns;
    int max_insns;
    pc_start = tb->pc;
    next_page_start = (pc_start & TARGET_PAGE_MASK) + TARGET_PAGE_SIZE;
    ctx.pc = pc_start;

    /* once we have GDB, the rest of the translate.c implementation should be
       ready for singlestep */
    ctx.singlestep_enabled = cs->singlestep_enabled;

    ctx.tb = tb;
    ctx.bstate = BS_NONE;

    ctx.mem_idx = cpu_mmu_index(env, false);
    num_insns = 0;
    max_insns = tb->cflags & CF_COUNT_MASK;
    if (max_insns == 0) {
        max_insns = CF_COUNT_MASK;
    }
    if (max_insns > TCG_MAX_INSNS) {
        max_insns = TCG_MAX_INSNS;
    }
    gen_tb_start(tb);

    while (ctx.bstate == BS_NONE) {
        tcg_gen_insn_start(ctx.pc);
        num_insns++;

        if (unlikely(cpu_breakpoint_test(cs, ctx.pc, BP_ANY))) {
            tcg_gen_movi_tl(cpu_pc, ctx.pc);
            ctx.bstate = BS_BRANCH;
            gen_helper_raise_exception_debug(cpu_env);
            /* The address covered by the breakpoint must be included in
               [tb->pc, tb->pc + tb->size) in order to for it to be
               properly cleared -- thus we increment the PC here so that
               the logic setting tb->size below does the right thing.  */
            ctx.pc += 4;
            goto done_generating;
        }

        if (num_insns == max_insns && (tb->cflags & CF_LAST_IO)) {
            gen_io_start();
        }

        ctx.opcode = cpu_ldl_code(env, ctx.pc);
        decode_opc(env, &ctx);
        ctx.pc = ctx.next_pc;

        if (cs->singlestep_enabled) {
            break;
        }
        if (ctx.pc >= next_page_start) {
            break;
        }
        if (tcg_op_buf_full()) {
            break;
        }
        if (num_insns >= max_insns) {
            break;
        }
        if (singlestep) {
            break;
        }

    }
    if (tb->cflags & CF_LAST_IO) {
        gen_io_end();
    }
    if (cs->singlestep_enabled && ctx.bstate != BS_BRANCH) {
        if (ctx.bstate == BS_NONE) {
            tcg_gen_movi_tl(cpu_pc, ctx.pc);
        }
        gen_helper_raise_exception_debug(cpu_env);
    } else {
        switch (ctx.bstate) {
        case BS_STOP:
            gen_goto_tb(&ctx, 0, ctx.pc);
            break;
        case BS_NONE: /* handle end of page - DO NOT CHAIN. See gen_goto_tb. */
            tcg_gen_movi_tl(cpu_pc, ctx.pc);
            tcg_gen_exit_tb(0);
            break;
        case BS_BRANCH: /* ops using BS_BRANCH generate own exit seq */
        default:
            break;
        }
    }
done_generating:
    gen_tb_end(tb, num_insns);
    tb->size = ctx.pc - pc_start;
    tb->icount = num_insns;

#ifdef DEBUG_DISAS
    if (qemu_loglevel_mask(CPU_LOG_TB_IN_ASM)
        && qemu_log_in_addr_range(pc_start)) {
        qemu_log("IN: %s\n", lookup_symbol(pc_start));
        log_target_disas(cs, pc_start, ctx.pc - pc_start, 0);
        qemu_log("\n");
    }
#endif
}

void riscv_cpu_dump_state(CPUState *cs, FILE *f, fprintf_function cpu_fprintf,
                         int flags)
{
    RISCVCPU *cpu = RISCV_CPU(cs);
    CPURISCVState *env = &cpu->env;
    int i;

    cpu_fprintf(f, "pc=0x" TARGET_FMT_lx "\n", env->pc);
    for (i = 0; i < 32; i++) {
        cpu_fprintf(f, " %s " TARGET_FMT_lx, regnames[i], env->gpr[i]);
        if ((i & 3) == 3) {
            cpu_fprintf(f, "\n");
        }
    }

#ifndef CONFIG_USER_ONLY
    cpu_fprintf(f, " %s " TARGET_FMT_lx "\n", "MSTATUS ",
                env->mstatus);
    cpu_fprintf(f, " %s " TARGET_FMT_lx "\n", "MIP     ", env->mip);
    cpu_fprintf(f, " %s " TARGET_FMT_lx "\n", "MIE     ", env->mie);
#endif

    for (i = 0; i < 32; i++) {
        if ((i & 3) == 0) {
            cpu_fprintf(f, "FPR%02d:", i);
        }
        cpu_fprintf(f, " %s %016" PRIx64, fpr_regnames[i], env->fpr[i]);
        if ((i & 3) == 3) {
            cpu_fprintf(f, "\n");
        }
    }
}


void riscv_tcg_init(void)
{
    int i;
    static int inited;

    /* Initialize various static tables. */
    if (inited) {
        return;
    }

    cpu_env = tcg_global_reg_new_ptr(TCG_AREG0, "env");

    /* WARNING: cpu_gpr[0] is not allocated ON PURPOSE. Do not use it. */
    /* Use the gen_set_gpr and gen_get_gpr helper functions when accessing */
    /* registers, unless you specifically block reads/writes to reg 0 */
    TCGV_UNUSED(cpu_gpr[0]);
    for (i = 1; i < 32; i++) {
        cpu_gpr[i] = tcg_global_mem_new(cpu_env,
                             offsetof(CPURISCVState, gpr[i]), regnames[i]);
    }

    for (i = 0; i < 32; i++) {
        cpu_fpr[i] = tcg_global_mem_new_i64(cpu_env,
                             offsetof(CPURISCVState, fpr[i]), fpr_regnames[i]);
    }

    cpu_pc = tcg_global_mem_new(cpu_env, offsetof(CPURISCVState, pc), "pc");
    load_res = tcg_global_mem_new(cpu_env, offsetof(CPURISCVState, load_res),
                             "load_res");

#ifdef CONFIG_USER_ONLY
    cpu_amoinsn = tcg_global_mem_new_i32(cpu_env,
                    offsetof(CPURISCVState, amoinsn),
                    "amoinsn");
#endif
    inited = 1;
}
