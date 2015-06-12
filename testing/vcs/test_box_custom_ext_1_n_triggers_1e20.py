import vcs
x=vcs.init()
b=x.createboxfill()
b.boxfill_type="custom"
b.levels=[10.0, 21.42857142857143, 32.85714285714286, 44.28571428571429, 55.71428571428572, 67.14285714285715, 78.57142857142858, 90.00000000000001]
assert(abs(b.levels[0])<1.e19 and b.ext_1 is False and b.ext_2 is False)
b.ext_1=False
assert(abs(b.levels[0])<1.e19 and b.ext_1 is False and b.ext_2 is False)
