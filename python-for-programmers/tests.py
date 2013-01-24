import subprocess

def test_build_talk():
    subprocess.call('pandoc -t beamer -V theme:Warsaw talk.md -o ../talk.pdf'.split())
