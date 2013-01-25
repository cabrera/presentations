import subprocess

def test_build_talk():
    subprocess.call('pandoc -t beamer -V theme:Warsaw --template=my.beamer talk.md -o ../talk.pdf'.split())
