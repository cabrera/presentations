import subprocess

def test_build_pdf():
    subprocess.call('pandoc README.md -o README.pdf'.split())
