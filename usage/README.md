
## Usage (2024/09/19)

### Setup Variables

```
tlmgr conf texmf TEXINPUTS.aplatex "$TEXMFDOTDIR;$TEXMF/tex/{uplatex,platex,latex,generic,}//"
tlmgr conf texmf TEXINPUTS.aplatex-dev "$TEXMFDOTDIR;$TEXMF/tex/{latex-dev,uplatex,platex,latex,generic,}//"
tlmgr conf texmf TEXINPUTS.aptex "$TEXMFDOTDIR;$TEXMF/tex/{uptex,ptex,plain,generic,latex,}//"
```

### Generate All Formats

```
cd usage
fmtutil-sys --cnffile fmt-aptex.cnf --all
```

### Compile Your Document


```
ptex-ng +[fmt] doc.tex
```

`fmt`: `aplatex`, `platex-ng`, `platex-dev-ng`

