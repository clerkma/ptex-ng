from bs4 import BeautifulSoup

# https://learn.microsoft.com/en-us/typography/opentype/spec/languagetags
# https://learn.microsoft.com/en-us/typography/opentype/spec/scripttags

out = []
with open("language-tags.html", encoding="U8") as src:
    soup = BeautifulSoup(src, "lxml")
if soup:
    language = []
    for table in soup.find_all("table"):
        for row in table.find_all("tr"):
            col = [x.text for x in row.find_all("td")]
            if len(col) == 3:
                tag = col[1].replace("'", "")
                language.append((tag, col[0]))
    out.append("$OT_TAG_LANGUAGE = {")
    for ent in language:
        out.append('  "%.4s" => "%s",' % (ent[0], ent[1]))
    out.append("}")


with open("script-tags.html", encoding="U8") as src:
    soup = BeautifulSoup(src, "lxml")
if soup:
    script = []
    for table in soup.find_all("table"):
        for row in table.find_all("tr"):
            col = [x.text for x in row.find_all("td")]
            if len(col) == 3:
                tag = col[1].replace("'", "")
                i= -1
                for idx, val in enumerate(script):
                    if val[0] == tag:
                        i = idx
                        break
                if i == -1:
                    script.append([tag, col[0]])
                else:
                    script[i][1] += "/" + col[0]
    out.append("$OT_TAG_SCRIPT = {")
    for ent in script:
        out.append('  "%.4s" => "%s",' % (ent[0], ent[1]))
    out.append("}")
with open("tag.rb", "wb") as dst:
    dst.write("\n".join(out).encode("U8"))
