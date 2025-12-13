import freetype
import struct
import pathlib
import io
import zlib
import argparse

UNITY = 0o4000000
U32MAX = 0xFFFFFFFF
FIXWORD_A = 0xFF000000

def make_fixword(val):
    if val >= 0:
        return int(val * UNITY) & U32MAX
    else:
        return (int((val + 16) * UNITY) & U32MAX) | FIXWORD_A

def make_header():
    pass

def make_metric(face):
    wd = []
    ht = []
    dp = []
    for x in range(256):
        g = face.get_char_index(x)
        face.load_glyph(g, freetype.FT_LOAD_NO_SCALE)
        outline = face.glyph.outline
        wd.append(face.glyph.advance.x)
        if outline.contours:
            bbox = outline.get_bbox()
            ht.append(max(bbox.yMax, 0))
            dp.append(abs(min(bbox.yMin, 0)))
        else:
            ht.append(0)
            dp.append(0)
    return wd, ht, dp

def store_kern(prog, kern, value, skip_op=0):
    prog += [skip_op, value[0], 0x80, len(kern)]
    kern.append(value[1])

def get_kern(face):
    pair = {}
    remainder = {}
    count = 0
    upm = face.units_per_EM
    for l in range(1, 256):
        save_count = count
        for r in range(1, 256):
            k = face.get_kerning(l, r, freetype.FT_KERNING_UNSCALED).x
            if k and count < 256:
                pair.setdefault(l, []).append((r, make_fixword(k / upm)))
                count += 1
        if count != save_count:
            remainder[l] = save_count
    prog = []
    kern = []
    for key in sorted(pair):
        val = pair[key]
        for one_val in val[:-1]:
            store_kern(prog, kern, one_val)
        store_kern(prog, kern, val[-1], 0x80)
    blob_prog = bytes(prog)
    blob_kern = struct.pack(f">{len(kern)}L", *kern)
    return remainder, blob_prog, blob_kern

def read_file(path):
    data = path.read_bytes()
    hash = zlib.crc32(data)
    return freetype.Face(io.BytesIO(data)), hash

def main(src, out):
    path = pathlib.Path(src)
    try:
        if result := read_file(path):
            face, hash = result
            upm = face.units_per_EM
            lh = 2
            header_checksum = hash
            header_design_size = make_fixword(10.0)
            wd, ht, dp = make_metric(face)
            remainder, blob_nl, blob_nk = get_kern(face)
            bc = 0
            ec = 255
            nw = 256
            nh = 2
            nd = 2
            ni = 2
            nl = len(blob_nl) // 4
            nk = len(blob_nk) // 4
            ne = 0
            np = 7
            slant = make_fixword(0)
            space = make_fixword(wd[ord(' ')] / upm)
            stretch = make_fixword(0.16)
            shrink = make_fixword(0.11)
            xheight = make_fixword(ht[ord('x')] / upm)
            quad = make_fixword(1.0)
            extra_space = make_fixword(0.1)
            lf = 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np
            blob_genesis = struct.pack(">12H", lf, lh, bc, ec, nw, nh, nd, ni, nl, nk, ne, np)
            blob_header = struct.pack(">LL", header_checksum, header_design_size)
            char_info = []
            for x in range(256):
                char_info += [x, 16 + 1]
                if x in remainder:
                    char_info += [1, remainder[x]]
                else:
                    char_info += [0, 0]
            blob_char_info = bytes(char_info)
            data_wd = [make_fixword(x / upm) for x in wd]
            blob_nw = struct.pack(">256L", 0, *data_wd[1:])
            blob_nh = struct.pack(">LL", 0, make_fixword(max(ht) / upm))
            blob_nd = struct.pack(">LL", 0, make_fixword(max(dp) / upm))
            blob_ni = struct.pack(">LL", 0, 0)
            blob_param = struct.pack(">7L", slant, space, stretch, shrink, xheight, quad, extra_space)
            blob_final = (
                blob_genesis + blob_header + blob_char_info
                + blob_nw + blob_nh + blob_nd + blob_ni
                + blob_nl + blob_nk + blob_param
            )
            pathlib.Path(out).write_bytes(blob_final)
    except Exception as e:
        print("Error:", e)
        print(f"Please check your font: {src} ...")

def run_test():
    main("C:\\Windows\\Fonts\\times.ttf", "times.tfm")
    main("C:\\Windows\\Fonts\\timesi.ttf", "timesi.tfm")
    main("C:\\Windows\\Fonts\\palab.ttf", "palab.tfm")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='otf2tfm',
        description='dump OpenType data to TFM',
    )
    parser.add_argument("-s", "--src", type=str, help="source path of OpenType file")
    parser.add_argument("-o", "--out", type=str, help="output path of TFM file")
    task = parser.parse_args()

    if task.src and task.out:
        if task.src == task.out:
            print("Please check your command line")
        else:
            print(task.src, task.out)
            main(task.src, task.out)
