"""
 *  Copyright (c) 2017 Bastian Koppelmann C-Lab/University Paderborn
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
"""

import yaml
import sys
import collections, operator
PREFIX_FILE = 'prefixes.yaml'
INST_FILE = 'instructions.yaml'
FORMAT_FILE = 'formats.yaml'
OUTPUT_FILE = '../insn-decoder.h'

"""
Helper functions
"""
def bitlen(val):
    return len(bin(val))-2

def str_to_int(val):
    if type(val) == int:
        return val
    try:
        return int(val, 2)
    except ValueError:
        try:
            return int(val, 10)
        except ValueError:
            sys.exit("Conversion of '{}' not possible".format(val))

def pick_qemu_extract_func(length):
    if length <= 32:
        return "extract32"
    elif length <= 64:
        return "extract64"
    else:
        sys.exit("Error: QEMU doesn't support insn-length > 64 yet")

"""
Parser functions
"""

def parse_range(token):
    tmp = token.split("..")
    hi=int(tmp[0])
    lo=int(tmp[1])
    if hi <= lo:
        sys.exit("bad range %d..%d" % (hi,lo))

    return (hi, lo)

def parse_range_list(l):
    result = list()
    for i, r in enumerate(l):
        se = ''
        if 's' in str(r):
            r = r.replace('s','')
            se = 's'
        if '..' in str(r):
            end, start = parse_range(r)
        else:
            start = int(r)
            end = start + 1
        result.append({'start': start, 'len': end-start+1, 'signext':se})
    return result


def parse_prefixes():
    with open(PREFIX_FILE, 'r') as f:
        data = yaml.load(f.read())
        # check if bitmask contains '~'
        for key, value in data.iteritems():
            if '~' in str(value['bitmask']):
                value['bitmask'] = str_to_int(value['bitmask'].replace('~', ''))
                value['comperator'] = '!'
            else:
                value['comperator'] = '='

            if value['len'] % 8 != 0:
                sys.exit('Length not dividable by 8')

            value['pc_inc'] = value['len'] / 8

        return data

def parse_formats():
    with open(FORMAT_FILE, 'r') as f:
        data = yaml.load(f.read())
        # convert ranges into start, len and set signexted flag
        for immediate in data['immediates']:
            name = immediate.keys()[0]
            new_immediate = dict()
            for value in immediate.values():
                imm_ranges = value['data']
                value['data'] = parse_range_list(imm_ranges)

        for field in data['fields']:
            for key, value in field.iteritems():
                end, start = parse_range(value)
                field[key] = {'start': start, 'len': end-start+1}
    return data

def parse_instructions():
    result = dict()
    with open(INST_FILE, 'r') as f:
        data = yaml.load(f.read())
        # convert ranges into start, len
        for op in data['opcodes']:
            for key, value in op.iteritems():
                end, start = parse_range(value)
                op[key] = {'start': start, 'len': end-start+1}

    return data

"""

Code gen functions

"""

def gen_header(f):
    f.write('/*\n')
    f.write(' * THIS FILE IS GENERATED, DO NOT EDIT IT MANUALLY\n')
    f.write(' * (C) 2017 Bastian Koppelmann <kbastian@mail.upb.de>\n')
    f.write(' */\n\n')

def gen_prefix_code(f, prefix_data):
    f.write("static void decode_opc(CPURISCVState *env, DisasContext *ctx)\n{\n")

    for i, (key, value) in enumerate(prefix_data.iteritems()):

        if_string = "if " if i == 0 else "} else if "
        insn_len = value['len']
        bitmask = value['bitmask']
        comperator = value['comperator']
        pc_inc = value['pc_inc']

        f.write("    {} ({}(ctx->opcode, 0, {}) {}= {}) {{\n".format(
                if_string, pick_qemu_extract_func(insn_len), bitlen(bitmask),
                comperator, bitmask))
        f.write("        decode_{}(env, ctx);\n".format(key))
        f.write("        ctx->next_pc = ctx->pc + {};\n".format(pc_inc))

    f.write("    } else {\n")
    f.write("        kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);\n")
    f.write("    }\n")
    f.write("}\n")

def gen_immediate_code(f, data):
    def get_fill_length(immediate):
        fill = immediate.values()[0]['fill']
        if fill != None:
            if fill == len(fill) * '0':
                return len(fill)
            else:
                sys.exit("Error: found '{}', but only '0' values are allowed for 'fill' field".format(fill))
        return 0

    for immediate in data:
        name = immediate.keys()[0]
        values = immediate[name]
        pos = get_fill_length(immediate)

        f.write("#define GET_{}_IMM(insn) \\\n".format(name))
        fields = values["data"]
        for i, field in enumerate(reversed(fields)):
            or_field = "|" if i > 0 else " "
            f.write("    {} ({}extract{}(insn, {}, {}) << {})\\\n"
                    .format(or_field, field["signext"], 32, field["start"], field["len"],
                            pos)
                   )
            pos += field["len"]
        f.write("\n")

def gen_insn_format_code(f, data):
    def find_field(field_name, field_data):
        for f in field_data:
            name = f.keys()[0]
            if field_name == name:
                return f[name]

    fields = data['fields']
    immediates = data['immediates']
    formats = data['formats']

    f.write("typedef struct {\n")
    for field in fields:
        name = field.keys()[0]
        emitType = "uint32_t"
        prefix = 'field'

        f.write("    {} {}_{};\n"
                .format(emitType, prefix, name)
               )
    f.write('\n')
    for immediate in immediates:
        name = immediate.keys()[0]
        emitType = "uint32_t"
        prefix = 'immediate'
        f.write("    {} {}_{};\n"
                    .format(emitType, prefix, name)
               )
    f.write("} DisasFormats;\n\n")
    f.write('#else\n\n')

    for format_val in formats:
        name = format_val.keys()[0]
        values = format_val[name]
        fields_ref = values['fields'] if 'fields' in values else list()
        imm_ref = values['immediates'] if 'immediates' in values else list()

        f.write("static void decode_format_{}(DisasContext *ctx)\n{{\n"
                .format(name))

        for field_name in fields_ref:
            field_data = find_field(field_name, fields)
            f.write("    ctx->fmt.{}_{} = extract32(ctx->opcode, {}, {});\n"
                         .format('field', field_name, field_data['start'],
                                 field_data['len']))
        for imm in imm_ref:
            f.write("    ctx->fmt.{}_{} = GET_{}_IMM(ctx->opcode);\n"
                    .format('immediate', imm, imm)
                   )

        f.write('}\n\n')

def gen_format_code(f, data):
    f.write('#ifdef RISCV_DECODE_HEADER\n')
    gen_immediate_code(f, data['immediates'])
    gen_insn_format_code(f, data)

def transform_insn_data(insn_data, format_data, prefix_data):

    def get_first_opcode(prefix):
        opcode_checklist = dict()
        for opcode in insn_data['opcodes']:
            name = opcode.keys()[0]
            opcode_checklist[name] = 0

        for insn in insn_data['instructions']:
            value = insn.values()[0]

            if value['prefix'] != prefix:
                continue

            for opcode in value['opcodes']:
                opcode_checklist[opcode] += 1

        return max(opcode_checklist, key=opcode_checklist.get)

    def find_next_opcode(prefix, major_opcode_dict):
        opcode_check = dict()
        for opcode in insn_data['opcodes']:
            name = opcode.keys()[0]
            opcode_check[name] = 0

        for insn in insn_data['instructions']:
            value = insn.values()[0]
            if value['prefix'] != prefix:
                continue
            op_names = value['opcodes']
            if major_opcode_dict.viewitems() <= op_names.viewitems():
                for op in op_names:
                    if op not in major_opcode_dict:
                        opcode_check[op] += 1
        return max(opcode_check, key=opcode_check.get)

    def insert_into_opcode_tree(p, result, call_string, major, curr_op_val, insn):
        if call_string.viewitems() == insn['opcodes'].viewitems(): # we found a insn heroe
            result[curr_op_val] = insn
        else:
            minor_opcode = find_next_opcode(p, call_string)
            value = insn['opcodes'][minor_opcode]
            new_call_string = call_string.copy()
            new_call_string[minor_opcode] = insn['opcodes'][minor_opcode]
            if not curr_op_val in result.keys():
                s, l = find_start_len(minor_opcode)
                result[curr_op_val] = {'opcode': minor_opcode, 'start': s, 'len': l, 'oplist': {}}

            insert_into_opcode_tree(p, result[curr_op_val]['oplist'], new_call_string, minor_opcode, value, insn)

    def find_start_len(name):
        for op in insn_data['opcodes']:
            key, value = op.items()[0]
            if key == name:
                return (value['start'], value['len'])

    result = dict()
    for p in prefix_data:
        major_opcode = get_first_opcode(p)
        if major_opcode == None:
            continue
        s, l = find_start_len(major_opcode)
        result[p] = {'opcode': major_opcode, 'start': s, 'len': l, 'oplist':{}}

        for insn in insn_data['instructions']:
            name, insn_dat = insn.items()[0]
            insn_dat['name'] = name
            if insn_dat['prefix'] != p:
                continue
            insert_into_opcode_tree(p, result[p]['oplist'], {major_opcode: insn_dat['opcodes'][major_opcode]},
                                    major_opcode,
                                    insn_dat['opcodes'][major_opcode],
                                    insn_dat)

    return result


def gen_insn_code(f, insn_decoder_data):

    def gen_switch_tree(indent, opdict):
        indentStr = " " * (indent)
        indentStr4 = " " * (indent + 4)

        for opval, opdata in opdict.items():
            f.write(indentStr + "case {}:\n".format(opval))

            if 'oplist' in opdata:
                name = opdata['opcode']
                start = opdata['start']
                length = opdata['len']
                f.write(indentStr4 + "/* Decode {} */\n".format(name))
                f.write(indentStr4 + "switch (extract32(ctx->opcode, {}, {})) {{\n"
                                     .format(start, length))
                gen_switch_tree(indent + 4, opdata['oplist'])
            else:
                insn_name = opdata['name']
                insn_format = opdata['format']
                insn_func = opdata['func']
                f.write(indentStr4 + "/* {} */\n".format(insn_name))
                f.write(indentStr4 + "decode_format_{}(ctx);\n".format(insn_format))
                f.write(indentStr4 + "{}(env, ctx);\n".format(insn_func))

            f.write(indentStr4 + "break;\n")
        f.write(indentStr + "default:\n")
        f.write(indentStr + "    kill_unknown(ctx, RISCV_EXCP_ILLEGAL_INST);\n")
        f.write(indentStr + "    break;\n")
        f.write(indentStr + "}\n")

    for prefix, data in insn_decoder_data.iteritems():
        f.write('static void decode_{}(CPURISCVState *env, DisasContext *ctx)\n{{\n'
                .format(prefix))
        if len(data['oplist']) < 1:
            f.write('}\n\n')
            continue
        indent = 4
        indentStr = ' ' * 4
        f.write(indentStr + "/* Decode {} */\n".format(data['opcode']))
        f.write(indentStr + "uint32_t {} = extract32(ctx->opcode, {}, {});\n\n"
                .format(data['opcode'].lower(), data['start'], data['len']))
        f.write(indentStr + "switch ({}) {{\n".format(data['opcode'].lower()))
        gen_switch_tree(indent, data['oplist'])

        f.write("}\n")

"""
Main
"""

def main():
    prefix_data = parse_prefixes()
    format_data = parse_formats()
    insn_data = parse_instructions()

    insn_decoder_data = transform_insn_data(insn_data, format_data, prefix_data)

    with open(OUTPUT_FILE, 'w') as f:
        gen_header(f)
        gen_format_code(f, format_data)
        gen_insn_code(f, insn_decoder_data)
        gen_prefix_code(f, prefix_data)
        f.write('#endif')
main()
