set errorformat=%tRROR:\ %m\ at\ %f:%l,%-G%.%#
set makeprg=julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\'
