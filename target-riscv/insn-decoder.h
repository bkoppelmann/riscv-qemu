/*
 * THIS FILE IS GENERATED, DO NOT EDIT IT MANUALLY
 * (C) 2017 Bastian Koppelmann <kbastian@mail.upb.de>
 */

#ifdef RISCV_DECODE_HEADER
#define GET_I_IMM(insn) \
      (extract32(insn, 20, 5) << 0)\
    | (sextract32(insn, 25, 7) << 5)\

#define GET_S_IMM(insn) \
      (extract32(insn, 7, 5) << 0)\
    | (sextract32(insn, 25, 7) << 5)\

#define GET_B_IMM(insn) \
      (extract32(insn, 8, 4) << 1)\
    | (extract32(insn, 25, 6) << 5)\
    | (extract32(insn, 7, 2) << 11)\
    | (sextract32(insn, 31, 2) << 13)\

#define GET_U_IMM(insn) \
      (extract32(insn, 12, 8) << 12)\
    | (extract32(insn, 20, 12) << 20)\

#define GET_J_IMM(insn) \
      (extract32(insn, 21, 4) << 1)\
    | (extract32(insn, 25, 6) << 5)\
    | (extract32(insn, 20, 2) << 11)\
    | (extract32(insn, 12, 8) << 13)\
    | (sextract32(insn, 31, 2) << 21)\

typedef struct {
    uint32_t field_rd;
    uint32_t field_rs1;
    uint32_t field_rs2;

    uint32_t immediate_I;
    uint32_t immediate_S;
    uint32_t immediate_B;
    uint32_t immediate_U;
    uint32_t immediate_J;
} DisasFormats;

#else

static void decode_format_R(DisasContext *ctx)
{
    ctx->fmt.field_rd = extract32(ctx->opcode, 7, 5);
    ctx->fmt.field_rs1 = extract32(ctx->opcode, 15, 5);
    ctx->fmt.field_rs2 = extract32(ctx->opcode, 20, 5);
}

static void decode_format_I(DisasContext *ctx)
{
    ctx->fmt.field_rd = extract32(ctx->opcode, 7, 5);
    ctx->fmt.field_rs1 = extract32(ctx->opcode, 15, 5);
    ctx->fmt.immediate_I = GET_I_IMM(ctx->opcode);
}

static void decode_format_S(DisasContext *ctx)
{
    ctx->fmt.field_rs1 = extract32(ctx->opcode, 15, 5);
    ctx->fmt.field_rs2 = extract32(ctx->opcode, 20, 5);
    ctx->fmt.immediate_S = GET_S_IMM(ctx->opcode);
}

static void decode_format_SB(DisasContext *ctx)
{
    ctx->fmt.field_rs1 = extract32(ctx->opcode, 15, 5);
    ctx->fmt.field_rs2 = extract32(ctx->opcode, 20, 5);
    ctx->fmt.immediate_B = GET_B_IMM(ctx->opcode);
}

static void decode_format_U(DisasContext *ctx)
{
    ctx->fmt.field_rd = extract32(ctx->opcode, 7, 5);
    ctx->fmt.immediate_U = GET_U_IMM(ctx->opcode);
}

static void decode_format_UJ(DisasContext *ctx)
{
    ctx->fmt.field_rd = extract32(ctx->opcode, 7, 5);
    ctx->fmt.immediate_J = GET_J_IMM(ctx->opcode);
}

static void decode_32Bit(CPURISCVState *env, DisasContext *ctx)
{
    /* Decode OP1_32 */
    uint32_t op1_32 = extract32(ctx->opcode, 0, 7);

    switch (op1_32) {
    case 19:
        /* Decode FUNC3_32 */
        switch (extract32(ctx->opcode, 12, 3)) {
        case 0:
            /* ADDI */
            decode_format_I(ctx);
            gen_addi(env, ctx);
            break;
        case 1:
            /* Decode FUNC7_32 */
            switch (extract32(ctx->opcode, 25, 7)) {
            case 0:
                /* SLLI */
                decode_format_R(ctx);
                gen_slli(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        case 2:
            /* SLTI */
            decode_format_I(ctx);
            gen_slti(env, ctx);
            break;
        case 3:
            /* SLTIU */
            decode_format_I(ctx);
            gen_sltiu(env, ctx);
            break;
        case 4:
            /* XORI */
            decode_format_I(ctx);
            gen_xori(env, ctx);
            break;
        case 5:
            /* Decode FUNC7_32 */
            switch (extract32(ctx->opcode, 25, 7)) {
            case 0:
                /* SRLI */
                decode_format_R(ctx);
                gen_srli(env, ctx);
                break;
            case 32:
                /* SRAI */
                decode_format_R(ctx);
                gen_srai(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        case 6:
            /* ORI */
            decode_format_I(ctx);
            gen_ori(env, ctx);
            break;
        case 7:
            /* ANDI */
            decode_format_I(ctx);
            gen_andi(env, ctx);
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 35:
        /* Decode FUNC3_32 */
        switch (extract32(ctx->opcode, 12, 3)) {
        case 0:
            /* SB */
            decode_format_S(ctx);
            gen_sb(env, ctx);
            break;
        case 1:
            /* SH */
            decode_format_S(ctx);
            gen_sh(env, ctx);
            break;
        case 2:
            /* SW */
            decode_format_S(ctx);
            gen_sw(env, ctx);
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 3:
        /* Decode FUNC3_32 */
        switch (extract32(ctx->opcode, 12, 3)) {
        case 0:
            /* LB */
            decode_format_I(ctx);
            gen_lb(env, ctx);
            break;
        case 1:
            /* LH */
            decode_format_I(ctx);
            gen_lh(env, ctx);
            break;
        case 2:
            /* LW */
            decode_format_I(ctx);
            gen_lw(env, ctx);
            break;
        case 4:
            /* LBU */
            decode_format_I(ctx);
            gen_lbu(env, ctx);
            break;
        case 5:
            /* LHU */
            decode_format_I(ctx);
            gen_lhu(env, ctx);
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 51:
        /* Decode FUNC7_32 */
        switch (extract32(ctx->opcode, 25, 7)) {
        case 0:
            /* Decode FUNC3_32 */
            switch (extract32(ctx->opcode, 12, 3)) {
            case 0:
                /* ADD */
                decode_format_R(ctx);
                gen_add(env, ctx);
                break;
            case 1:
                /* SLL */
                decode_format_R(ctx);
                gen_sll(env, ctx);
                break;
            case 2:
                /* SLT */
                decode_format_R(ctx);
                gen_slt(env, ctx);
                break;
            case 3:
                /* SLTU */
                decode_format_R(ctx);
                gen_sltu(env, ctx);
                break;
            case 4:
                /* XOR */
                decode_format_R(ctx);
                gen_xor(env, ctx);
                break;
            case 5:
                /* SRL */
                decode_format_R(ctx);
                gen_srl(env, ctx);
                break;
            case 6:
                /* OR */
                decode_format_R(ctx);
                gen_or(env, ctx);
                break;
            case 7:
                /* AND */
                decode_format_R(ctx);
                gen_and(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        case 32:
            /* Decode FUNC3_32 */
            switch (extract32(ctx->opcode, 12, 3)) {
            case 0:
                /* SUB */
                decode_format_R(ctx);
                gen_sub(env, ctx);
                break;
            case 5:
                /* SRA */
                decode_format_R(ctx);
                gen_sra(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 103:
        /* Decode FUNC3_32 */
        switch (extract32(ctx->opcode, 12, 3)) {
        case 0:
            /* JALR */
            decode_format_I(ctx);
            gen_jalr(env, ctx);
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 55:
        /* LUI */
        decode_format_U(ctx);
        gen_lui(env, ctx);
        break;
    case 111:
        /* JAL */
        decode_format_UJ(ctx);
        gen_jal(env, ctx);
        break;
    case 115:
        /* Decode FUNC3_32 */
        switch (extract32(ctx->opcode, 12, 3)) {
        case 0:
            /* Decode FENCEI */
            switch (extract32(ctx->opcode, 20, 12)) {
            case 0:
                /* ECALL */
                decode_format_I(ctx);
                gen_ecall(env, ctx);
                break;
            case 1:
                /* EBREAK */
                decode_format_I(ctx);
                gen_ebreak(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        case 1:
            /* CSRRW */
            decode_format_I(ctx);
            gen_csrrw(env, ctx);
            break;
        case 2:
            /* CSRRS */
            decode_format_I(ctx);
            gen_csrrs(env, ctx);
            break;
        case 3:
            /* CSRRC */
            decode_format_I(ctx);
            gen_csrrc(env, ctx);
            break;
        case 5:
            /* CSRRWI */
            decode_format_I(ctx);
            gen_csrrwi(env, ctx);
            break;
        case 6:
            /* CSRRSI */
            decode_format_I(ctx);
            gen_csrrsi(env, ctx);
            break;
        case 7:
            /* CSRRCI */
            decode_format_I(ctx);
            gen_csrrci(env, ctx);
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 99:
        /* Decode FUNC3_32 */
        switch (extract32(ctx->opcode, 12, 3)) {
        case 0:
            /* BEQ */
            decode_format_SB(ctx);
            gen_beq(env, ctx);
            break;
        case 1:
            /* BNE */
            decode_format_SB(ctx);
            gen_bne(env, ctx);
            break;
        case 4:
            /* BLT */
            decode_format_SB(ctx);
            gen_blt(env, ctx);
            break;
        case 5:
            /* BGE */
            decode_format_SB(ctx);
            gen_bge(env, ctx);
            break;
        case 6:
            /* BLTU */
            decode_format_SB(ctx);
            gen_bltu(env, ctx);
            break;
        case 7:
            /* BGEU */
            decode_format_SB(ctx);
            gen_bgeu(env, ctx);
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    case 23:
        /* AUIPC */
        decode_format_U(ctx);
        gen_auipc(env, ctx);
        break;
    case 15:
        /* Decode FENCE2 */
        switch (extract32(ctx->opcode, 7, 13)) {
        case 0:
            /* Decode FENCE3 */
            switch (extract32(ctx->opcode, 27, 5)) {
            case 0:
                /* FENCE */
                decode_format_R(ctx);
                gen_fence(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        case 32:
            /* Decode FENCEI */
            switch (extract32(ctx->opcode, 20, 12)) {
            case 0:
                /* FENCEI */
                decode_format_R(ctx);
                gen_fence_i(env, ctx);
                break;
            default:
                kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
                break;
            }
            break;
        default:
            kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
            break;
        }
        break;
    default:
        kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
        break;
    }
}
static void decode_16Bit(CPURISCVState *env, DisasContext *ctx)
{
}

static void decode_opc(CPURISCVState *env, DisasContext *ctx)
{
    if  (extract32(ctx->opcode, 0, 2) == 3) {
        decode_32Bit(env, ctx);
        ctx->next_pc = ctx->pc + 4;
    } else if  (extract32(ctx->opcode, 0, 2) != 3) {
        decode_16Bit(env, ctx);
        ctx->next_pc = ctx->pc + 2;
    } else {
        kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);
    }
}
#endif