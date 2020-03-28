import math
def little_endian(big_endian): return "".join([big_endian[i:i+8][::-1] for i in range(0, len(big_endian), 8)])
hex2binmap = { "0": "0000", "1": "0001", "2": "0010", "3": "0011", "4": "0100", "5": "0101", "6": "0110", "7": "0111", "8": "1000", "9": "1001", "a": "1010", "b": "1011", "c": "1100", "d": "1101", "e": "1110", "f": "1111" }
def hex2bin(h, s = ""): return "".join(map(lambda i: hex2binmap[i], hex(h)[2:]))
def final_hash(hx): return "".join([hx[i-1]+hx[i] for i in range(len(hx)-1, -1, -2)])
def left_rotate(b, c): return (b << c) | (b >> (32-c))
def fix_hex_length(hex_string): return hex_string if len(hex_string) == 8 else "0"*(8-len(hex_string)) + hex_string
def md5(msg):
    t32 = pow(2, 32)
    mod_add = lambda a, b: (a + b) % t32
    K = [int(math.floor(t32*abs(math.sin(i+1)))) for i in range(64)]
    s = [7, 12, 17, 22] * 4 + [5,  9, 14, 20] * 4 + [4, 11, 16, 23] * 4 + [6, 10, 15, 21] * 4
    a0, b0, c0, d0 = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476
    bin_msg = "".join(map(hex2bin, list(msg.encode('utf-8'))))
    original_length = len(bin_msg)
    zeros = 448+(512-((original_length+1) % 512)) if (original_length+1) % 512 > 448 else 448 - ((original_length+1) % 512)
    bin_msg_length = bin(original_length%pow(2, 64))[2:][::-1]
    final_msg = little_endian(bin_msg + "1" + "0" * zeros) + bin_msg_length + "0"*(64-len(bin_msg_length))
    for j in range(0, len(final_msg), 512):
        chunk = final_msg[j:j+512]
        M = list(map(lambda b: int(b[::-1], 2), [chunk[m:m+32] for m in range(0, 512, 32)]))
        A, B, C, D = a0, b0, c0, d0
        for i in range(64):
            F, g = None, None
            if i >= 0 and i < 16: F, g = ((B & C) | ((~ B) & D)), i
            elif i >= 16 and i < 32: F, g = ((D & B) | ((~ D) & C)), (5*i + 1) % 16
            elif i >= 32 and i < 48: F, g = (B ^ C ^ D), (3*i + 5) % 16
            elif i >= 48 and i < 64: F, g = (C ^ (B | (~ D))), (7*i) % 16
            F = mod_add(mod_add(mod_add(F, A), K[i]), M[g])
            A, D, C, B = D, C, B, mod_add(B, left_rotate(F, s[i]))
        a0, b0, c0, d0 = mod_add(a0, A), mod_add(b0, B), mod_add(c0, C), mod_add(d0, D)
    return final_hash(fix_hex_length(hex(a0)[2:]))+final_hash(fix_hex_length(hex(b0)[2:]))+final_hash(fix_hex_length(hex(c0)[2:]))+final_hash(fix_hex_length(hex(d0)[2:]))