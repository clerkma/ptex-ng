# update texlive source (2024/01/13)
cd texlive
REV=`svn log | head -2 | tail -1 | cut -d" " -f1 | cut -d"r" -f2`
TRY=`svn up | tail -1`
CUR=`echo $TRY | cut -d" " -f1`

if [ "$CUR" = "At" ]; then
  echo "No update available."
fi

if [ "$CUR" = "Updated" ]; then
  echo "Updated."
  git add *
  VER=`echo $TRY | cut -d" " -f4`
  git commit -a -m "synchronized texlive $VER"
  git push
fi

