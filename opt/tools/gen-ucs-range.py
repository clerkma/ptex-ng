import os


def run_task():
    url = "https://unicode.org/Public/15.0.0/ucd/Blocks-15.0.0d3.txt"
    txt = url.split("/")[-1]
    if not os.path.exists(txt):
        os.system("curl -LO %s" % url)
    if not os.path.exists(txt):
        print("fail to get file '%s'." % txt)
    else:
        blocks = []
        with open(txt) as src:
            for line in src.readlines():
                line_body = line.replace("\n", "")
                if len(line_body) > 1 and line_body[0] != "#":
                    blocks.append(line_body.split("; "))
        max_block_name = max([len(x[1]) for x in blocks])
        for idx, ent in enumerate(blocks):
            block_range = ent[0].split("..")
            block_label = "/* {:<%d} */" % (max_block_name + 3)
            idx_tag = " /* 0x%02X */" % idx if idx % 16 == 0 else ""
            print("  0x%s, %s%s" % (block_range[0], block_label.format(ent[1]), idx_tag))


if __name__ == "__main__":
    run_task()

