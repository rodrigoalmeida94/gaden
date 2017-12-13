cd $1
convert -delay 50 -loop 0 $(ls sum_*_XY.png | sort -n -k1.5) sum_XY.gif
convert -delay 50 -loop 0 $(ls sum_*_XZ.png | sort -n -k1.5) sum_XZ.gif
convert -delay 50 -loop 0 $(ls sum_*_YZ.png | sort -n -k1.5) sum_YZ.gif
cd ..
