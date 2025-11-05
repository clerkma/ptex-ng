
# Management of Formats

##  By Hand

### Setup Variables

```
tlmgr conf texmf TEXINPUTS.aplatex "$TEXMFDOTDIR;$TEXMF/tex/{uplatex,platex,latex,generic,}//"
tlmgr conf texmf TEXINPUTS.aplatex-dev "$TEXMFDOTDIR;$TEXMF/tex/{latex-dev,uplatex,platex,latex,generic,}//"
tlmgr conf texmf TEXINPUTS.aptex "$TEXMFDOTDIR;$TEXMF/tex/{uptex,ptex,plain,generic,latex,}//"
```

### Generate All Formats

```
cd usage/static
fmtutil-sys --cnffile fmt-aptex.cnf --all
```

## By Script

### Setup Variables and Generate All Formats

```
python3 tool.py
```

# Compile Your Document

```
aptex +[fmt] doc.tex
```

`fmt`: `aplatex`, `platex-ng`, `platex-dev-ng`
