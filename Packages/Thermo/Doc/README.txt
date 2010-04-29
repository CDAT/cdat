This package is designed to draw thermodynamic diagrams using vcs

There is NO real documentation except for this little file and the test code 
distributed.
General Skew-T documentation from the Air-Force manual can be found at:
http://meteorology.lyndonstate.edu/common/edulinks/AF_SkewT_manual.pdf

Modification/enhancement to the code are welcomed and should be addressed to:
doutriaux1@llnl.gov
Request for improvements/changes are welcome to but are less likely 
to be answered ;)


Initialisation

th=thermo.Gth(vcs_object,name)


Predefined types of thermodynamic diagram
th.type='emagram'
th.type='tephigram'
th.type='stuve'
th.type='skewT' (default)
user defined type is also allowed (th.type='custom', see bellow for more details)
You can control skewness (default is -35)

Thermodiagrams component (background are:)
isotherms, isothermsfilled, isobars, dryadiabats, pseudoadiabats
They are all VCS isoline(/fill) method and can be redefined by the user, 
giving full control of the plot aspect

the datawc of the plot represent corner point of the plot:
## Temperatures at the bottom of the grap (in C)
th.datawc_x1=-50.
th.datawc_x2=50.
## Pressure at bottom and top of page (in hPa)
th.datawc_y1=1050.
th.datawc_y2=100.

## PLotting windbarbs is also possible and the windbarbs scale can be controlled with:
th.windbarbsscales=[5,2,1]
        ---------------------------------------------------------------
        \  |  /   /    /
         \ | /   /    /
          \|/   /
           V

           total length is 1
           barbs angle is 60 degrees
           barb depth is .1


CUSTOM Diagrams

You'll need to redefine the follwing functions
TP2XY and XY2TP
th.TP2XY=myTP2XY_func
TP2XY: Converts Temperature and pressure to X/Y Coord
XY2TP does the backward operation

Arguments to the functions are:
def myTP(self,T,P):
    ...
    retun X,Y


TO DO: CAPE Computation
