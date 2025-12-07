import freetype
import struct
import pathlib
import io
import zlib
import argparse

UNITY = 0o4000000

def make_fixword(val):
    return int(val * UNITY) & 0xFFFFFFFF

def make_header():
    pass

def make_metric(face):
    wd = []
    ht = []
    dp = []
    for x in range(256):
        g = face.get_char_index(x)
        face.load_glyph(g, freetype.FT_LOAD_NO_SCALE)
        cbox = face.glyph.outline.get_bbox()
        wd.append(face.glyph.advance.x)
        ht.append(cbox.yMax)
        dp.append(cbox.yMin)
    return wd, ht, dp

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
            bc = 0
            ec = 255
            nw = 256
            nh = 2
            nd = 2
            ni = 2
            nl = 0
            nk = 0
            ne = 0
            np = 7
            slant = make_fixword(0)
            space = make_fixword(wd[ord(' ')] / upm)
            stretch = make_fixword(0.16)
            shrink = make_fixword(0.11)
            xheight = make_fixword(ht[ord('x')] / upm)
            quad = make_fixword(1.0)
            extra_space = make_fixword(0.1)
            lf = 6 + 2 + (ec - bc + 1) + nw + nh + nd + ni + 7
            blob_genesis = struct.pack(">12H", lf, lh, bc, ec, nw, nh, nd, ni, nl, nk, ne, np)
            blob_header = struct.pack(">LL", header_checksum, header_design_size)
            blob_char_info = b"".join([struct.pack(">BBBB", x, 16 + 1, 0, 0) for x in range(256)])
            data_wd = [make_fixword(x / upm) for x in wd]
            blob_nw = struct.pack(">256L", 0, *data_wd[1:])
            blob_nh = struct.pack(">LL", 0, make_fixword(max(ht) / upm))
            blob_nd = struct.pack(">LL", 0, make_fixword(-min(dp) / upm))
            blob_ni = struct.pack(">LL", 0, 0)
            blob_param = struct.pack(">7L", slant, space, stretch, shrink, xheight, quad, extra_space)
            blob_final = blob_genesis + blob_header + blob_char_info + blob_nw + blob_nh + blob_nd + blob_ni + blob_param
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
        print(task.src, task.out)
        main(task.src, task.out)

