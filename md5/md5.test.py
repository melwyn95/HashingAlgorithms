import hashlib
from md5 import md5


def print_test(hash1, hash2, msg):
    if hash1 != hash2:
        print("Fix your md5 for ", msg)
        print(hash1)
        print(hash2)
    else: 
        print("---------------SUCCESS---------------")
        print(msg[:60] if len(msg) > 60 else msg)
        print(hash1)
        print(hash2)
        print("-------------------------------------")


def std_lib_md5(msg): return hashlib.md5(msg.encode('utf-8')).hexdigest()


msg = "The quick brown fox jumps over the lazy dog"
print_test(md5(msg), std_lib_md5(msg), msg)

msg = ""
print_test(md5(msg), std_lib_md5(msg), msg)

msg = "The quick brown fox jumps over the lazy dog" * 1000
print_test(md5(msg), std_lib_md5(msg), msg)

msg = "The quick brown fox jumps over the lazy dog."
print_test(md5(msg), std_lib_md5(msg), msg)
