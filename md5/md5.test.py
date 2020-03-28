import hashlib
from md5 import md5


def print_test(hash1, hash2, msg):
    if hash1 != hash2:
        print("Fix your md5 for ", msg)
        print(hash1)
        print(hash2)
    else: pass
        # print("---------------SUCCESS---------------")
        # print(msg[:60] if len(msg) > 60 else msg)
        # print(hash1)
        # print(hash2)
        # print("-------------------------------------")


def std_lib_md5(msg): return hashlib.md5(msg.encode('utf-8')).hexdigest()


msg = "The quick brown fox jumps over the lazy dog"
print_test(md5(msg), std_lib_md5(msg), msg)

msg = ""
print_test(md5(msg), std_lib_md5(msg), msg)

msg = "The quick brown fox jumps over the lazy dog" * 1000
print_test(md5(msg), std_lib_md5(msg), msg)

msg = "The quick brown fox jumps over the lazy dog."
print_test(md5(msg), std_lib_md5(msg), msg)

# Hash Collision
msg1 = "4dc968ff0ee35c209572d4777b721587d36fa7b21bdc56b74a3dc0783e7b9518afbfa200a8284bf36e8e4b55b35f427593d849676da0d1555d8360fb5f07fea2"
msg2 = "4dc968ff0ee35c209572d4777b721587d36fa7b21bdc56b74a3dc0783e7b9518afbfa202a8284bf36e8e4b55b35f427593d849676da0d1d55d8360fb5f07fea2"
print("Messages equal ? ", msg1 == msg2)
print_test(md5(msg1), std_lib_md5(msg2), "---")
