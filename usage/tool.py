import tempfile
import pathlib
import os, platform
import re

def check_one_program(path, name):
    if path.exists():
        for x in path.iterdir():
            if re.match(name, x.name):
                return True
    return False

def check_programs():
    check_task = {
        "aptex(.exe)?": 0,
        "kpsewhich(.exe)?": 0,
        "tlmgr(.bat)?": 0,
        "fmtutil-sys(.exe)?": 0
    }
    for x in os.environ["PATH"].split(os.pathsep):
        if not x:
            continue
        path = pathlib.Path(x)
        for k in check_task.keys():
            if check_one_program(path, k):
                check_task[k] += 1
    return all(check_task.values())

def check_variables():
    check_task = {
        "aplatex": 0,
        "aplatex-dev": 0,
        "aptex": 0,
    }
    # tlmgr conf texmf TEXINPUTS.aplatex
    # kpsewhich -var-value TEXINPUTS.aplatex
    for k in check_task.keys():
        if os.popen(f"kpsewhich -var-value TEXINPUTS.{k}").read().strip():
            check_task[k] += 1
    return all(check_task.values())

def update_variables():
    prefix = "$TEXMFDOTDIR" + os.pathsep
    update_task = {
        "aplatex": prefix + "$TEXMF/tex/{uplatex,platex,latex,generic,}//",
        "aplatex-dev": prefix + "$TEXMF/tex/{latex-dev,uplatex,platex,latex,generic,}//",
        "aptex": prefix + "$TEXMF/tex/{uptex,ptex,plain,generic,latex,}//"
    }
    for k, v in update_task.items():
        value = v
        if platform.system() in ["Linux", "Darwin"]:
            value = v.replace("$", "\\$")
        command = f"tlmgr conf texmf TEXINPUTS.{k} \"{value}\""
        os.system(command)

stub_config = r"""
\dpxoutput=1
\pdfpagewidth=210 true mm
\pdfpageheight=297 true mm
\pdfhorigin=1 true in
\pdfvorigin=1 true in
\endinput
"""
stub_aplatex = r"""
\begingroup  \catcode`\{=1  \catcode`\}=2%
  \immediate\write20{<<< making "uplatex (ApTeX) with Babel" format >>>}
\endgroup
\scrollmode
\input aptex-config.tex
\input uplatex.ltx
\endinput
"""
stub_aptex = r"""
\input aptex-config.tex
\input euptex.src
\dump
\endinput
"""

stub_amstex = r"""
\input aptex-config.tex
\input plain
\input amstex
\dump
\endinput
"""

stub_formats = """
# plain
aptex aptex language.def *aptex.ini
# AMSTeX
amstex aptex language.def *amstex.ini
# LaTeX
aplatex aptex language.dat *aplatex.ini
aplatex-dev aptex language.dat *aplatex.ini
"""

def make_formats():
    if check_programs():
        if not check_variables():
            update_variables()
        cwd = os.getcwd()
        with tempfile.TemporaryDirectory() as tempdir:
            root = pathlib.Path(tempdir)
            (root / "aptex.ini").write_text(stub_aptex)
            (root / "amstex.ini").write_text(stub_amstex)
            (root / "aplatex.ini").write_text(stub_aplatex)
            (root / "aptex-config.tex").write_text(stub_config)
            (root / "fmt-aptex.cnf").write_text(stub_formats)
            os.chdir(tempdir)
            os.system("fmtutil-sys --cnffile fmt-aptex.cnf --all")
            os.chdir(cwd)

if __name__ == "__main__":
    make_formats()
