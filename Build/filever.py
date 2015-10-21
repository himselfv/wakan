import argparse
import os

parser = argparse.ArgumentParser(
	description="Reads executable file version to environment variable")
parser.add_argument('filename', help='File to study')
parser.add_argument('--major', action='store_true', help='Store major version only (1.5)')
args = parser.parse_args()

from win32api import GetFileVersionInfo, LOWORD, HIWORD
def get_version_number(filename):
  info = GetFileVersionInfo(filename, "\\")
  ms = info['FileVersionMS']
  ls = info['FileVersionLS']
  return HIWORD(ms), LOWORD(ms), HIWORD(ls), LOWORD(ls)

ver = get_version_number(args.filename)
if args.major:
	text = str(ver[0]) + '.' + str(ver[1])
else:
	text = '.'.join([str (i) for i in ver])

print text