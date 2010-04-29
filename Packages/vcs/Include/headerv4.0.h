"%!PS-Adobe-3.0\n\
%%DocumentFonts: Times-Roman\n\
%%Pages: (atend)\n\
% Joe Sirott University of Washington\n\
% Modified for Pacific Marine Environmental Lab\n\
%\n\
%%EndComments\n\
/frpagesave save def\n\
\n\
% Procedures\n\
\n\
/gr {grestore} bind def\n\
/gs {gsave} bind def\n\
/sd {setdash} bind def\n\
/w {setlinewidth} bind def\n\
/l {doxform lineto} bind def\n\
/n {newpath} bind def\n\
/m {doxform moveto} bind def\n\
/s {scale} bind def\n\
/c {setlinecap} bind def\n\
/j {setlinejoin} bind def\n\
/r {restore} bind def\n\
/h {showpage} bind def\n\
/t {stroke} bind def\n\
/f {eofill} bind def\n\
/gcol {ct exch get aload pop} bind def\n\
/o {gcol setrgbcolor} bind def\n\
/b {                                    % x1 y1 x2 y2 Box\n\
        localdict begin\n\
            /yto exch def /xto exch def\n\
            /yto1 exch def /xto1 exch def\n\
            n xto yto m xto yto1 l xto1 yto1 l xto1 yto l closepath\n\
        end\n\
} bind def\n\
/bd {                                   % x1 y1 x2 y2\n\
        localdict begin                 % box with x1,y1 user coords, x2,y2 pts\n\
            /yto exch def /xto exch def\n\
            /yto1 exch def /xto1 exch def\n\
            n xto1 yto1 m \n\
            0 yto rlineto \n\
            xto 0 rlineto \n\
            0 yto neg rlineto closepath\n\
        end\n\
} bind def\n\
/inch {72 mul} bind def\n\
%%0.25 inch /xmargin exch def\n\
%%0.25 inch /ymargin exch def\n\
%%0.9 inch /xmargin2 exch def %right margin \n\
%%0.8 inch /ymargin2 exch def %top margin \n\
0. inch /xmargin exch def\n\
0. inch /ymargin exch def\n\
0. inch /xmargin2 exch def %right margin \n\
0. inch /ymargin2 exch def %top margin \n\
/setwindow {\n\
        /window exch def\n\
} bind def\n\
/setvp {\n\
        /vp exch def\n\
} bind def\n\
\n\
/printmatrix {\n\
        aload pop = = = = = =\n\
} bind def\n\
\n\
/doxform {                              % x y => x' y'\n\
        xform transform\n\
} bind def\n\
        \n\
/settransform {                         % uoff voff => /xform contains matrix\n\
        localdict begin\n\
            /voff exch def\n\
            /uoff exch def\n\
            pagewidth pagelength gt {/psize pagewidth def} {/psize pagelength def} ifelse\n\
            vp 0 get psize mul uoff add xmargin add /umin exch def\n\
            vp 1 get psize mul voff add ymargin add /vmin exch def\n\
            vp 2 get psize mul uoff add xmargin2 sub /umax exch def\n\
            vp 3 get psize mul voff add ymargin2 sub /vmax exch def\n\
            window 0 get /xmin exch def\n\
            window 1 get /ymin exch def\n\
            window 2 get /xmax exch def\n\
            window 3 get /ymax exch def\n\
            umin vmin matrix translate /xform exch def\n\
            umax umin sub xmax xmin sub div\n\
            vmax vmin sub ymax ymin sub div\n\
            matrix scale xform matrix concatmatrix /xform exch def\n\
            xmin neg ymin neg matrix translate xform matrix concatmatrix\n\
        end\n\
        /xform exch def\n\
} bind def\n\
            \n\
/pm {                                   % Polymarker output at x,y\n\
        localdict begin\n\
            1 1 bd fill\n\
        end\n\
} bind def\n\
\n\
/ctext {                                % Center text horizontally\n\
    dup stringwidth exch 0.5 mul neg 0 rmoveto pop\n\
} bind def\n\
\n\
/rtext {                                % Right justify text\n\
    dup stringwidth exch neg 0 rmoveto pop\n\
} bind def\n\
\n\
/htext {                        % Center text vertically.\n\
    localdict begin\n\
        gsave dup\n\
        currentpoint /y0 exch def pop\n\
        false charpath flattenpath pathbbox \n\
        /y2 exch def pop /y1 exch def pop\n\
        y2 y1 add 2 div y0 sub neg 0 exch\n\
        grestore\n\
        rmoveto\n\
    end\n\
} bind def\n\
\n\
/sf {                                   % Scale the font in GKS coordinates\n\
        dup doxform pagelength pagewidth lt {exch} if\n\
        pop /Times-Roman findfont exch scalefont setfont\n\
} bind def\n\
\n\
/center {                               % Center the plot\n\
    /aspect exch def\n\
    aspect 1.0 gt \n\
	{\n\
	  1 aspect div /xfraction exch def 1 /yfraction exch def\n\
	  pagewidth /pagexsize exch def pagelength /pageysize exch def\n\
	}\n\
	{\n\
	  1 /xfraction exch def aspect /yfraction exch def\n\
	  pagewidth /pageysize exch def pagelength /pagexsize exch def\n\
	} ifelse\n\
    xmargin xmargin2 add /m2 exch def\n\
    pagewidth m2 sub /pagesize exch def\n\
    pagexsize m2 sub pagesize xfraction mul sub 2 div /xoffset exch def\n\
    ymargin ymargin2 add /m2 exch def\n\
    pageysize m2 sub pagesize yfraction mul sub 2 div /yoffset exch def\n\
    xoffset yoffset settransform\n\
} bind def\n\
\n\
/Landscape { 90 rotate 0 exch translate} bind def\n\
\n\
% Variables\n\
/localdict 64 dict def\n\
/ct 256 array def\n\
/nct 256 array def\n\
/gct 256 array def\n\
/markersize 1.0 def\n\
/linewidthscale 1.0 def\n\
/charexpansion 1.0 def\n\
/charspacing 0.0 def\n\
/startmatrix matrix currentmatrix def           % Save the current transform matrix\n\
/pagewidth 8.5 inch def                         % Width of page in points\n\
/pagelength 11.0 inch def                               % Length of page in points\n\
\n\
% Initialization routines\n\
0 c\n\
1 j\n\
[0.0 0.0 1.0 1.0] setvp\n\
[0.0 0.0 1.0 1.0] setwindow\n\
0 0 settransform\n\
\n\
%%EndProlog\n\
% Patterns\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 2\n\
/BBox [0 0 4 4]\n\
/XStep 4\n\
/YStep 4\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  0 3 lineto\n\
  3 0 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P1 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 4 4]\n\
/XStep 4\n\
/YStep 4\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  0 2 lineto\n\
  2 2 lineto\n\
  2 0 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P2 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 4 4]\n\
/XStep 4\n\
/YStep 4\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  0 4 lineto\n\
  2 4 lineto\n\
  2 2 lineto\n\
  4 2 lineto\n\
  4 0 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P3 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 4 4]\n\
/XStep 4\n\
/YStep 4\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  0 2 lineto\n\
  2 2 lineto\n\
  2 0 lineto\n\
closepath fill\n\
2 2 moveto\n\
2 4 lineto\n\
4 4 lineto\n\
4 2 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P4 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 1 4]\n\
/XStep 1\n\
/YStep 4\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  0 3 lineto\n\
  1 3 lineto\n\
  1 0 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P5 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 4 1]\n\
/XStep 4\n\
/YStep 1\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  3 0 lineto\n\
  3 1 lineto\n\
  0 1 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P6 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 1 4]\n\
/XStep 1\n\
/YStep 4\n\
/PaintProc\n\
   { pop \n\
  0 3 moveto \n\
  1 3 lineto\n\
  1 4 lineto\n\
  0 4 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P7 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 4 1]\n\
/XStep 4\n\
/YStep 1\n\
/PaintProc\n\
   { pop \n\
  0 0 moveto \n\
  1 0 lineto\n\
  1 1 lineto\n\
  0 1 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P8 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 10 10]\n\
/XStep 10\n\
/YStep 10\n\
/myline2 { 0 0 moveto \n\
10 10 lineto \n\
closepath stroke \n\
} bind def \n\
/PaintProc\n\
   { pop \n\
myline2\n\
0 10 translate\n\
myline2\n\
0 -20 translate\n\
myline2\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P9 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 10 10]\n\
/XStep 10\n\
/YStep 10\n\
/myline2 { 0 0 moveto \n\
10 10 lineto \n\
closepath stroke \n\
} bind def \n\
/PaintProc\n\
   {  \n\
   pop \n\
5 w\n\
myline2\n\
0 10 translate\n\
myline2\n\
0 -20 translate\n\
myline2\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P10 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 10 10]\n\
/XStep 10\n\
/YStep 10\n\
/myline { 0 10 moveto \n\
10 0 lineto \n\
closepath stroke \n\
} bind def \n\
/PaintProc\n\
   { pop \n\
1 w\n\
myline \n\
0 10 translate\n\
myline \n\
0 -20 translate \n\
myline \n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P11 exch def\n\
<< /PatternType 1 \n\
/PaintType 2 \n\
/TilingType 1 \n\
/BBox [0 0 10 10] \n\
/XStep 10 \n\
/YStep 10 \n\
/myline { 0 10 moveto \n\
10 0 lineto \n\
closepath stroke \n\
} bind def \n\
/PaintProc \n\
   {  \n\
   pop \n\
5 w \n\
myline \n\
0 10 translate\n\
myline \n\
0 -20 translate \n\
myline \n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P12 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 6 6]\n\
/XStep 6\n\
/YStep 6\n\
/PaintProc\n\
   {  \n\
   pop \n\
0 0 moveto\n\
0 4 lineto\n\
4 4 lineto\n\
4 0 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P13 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 8 8]\n\
/XStep 8\n\
/YStep 8\n\
/PaintProc\n\
   {  \n\
  pop \n\
newpath\n\
0 4 moveto\n\
4 8 lineto\n\
8 4 lineto\n\
4 0 lineto\n\
closepath fill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P14 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 9 1]\n\
/XStep 9\n\
/YStep 1\n\
/PaintProc\n\
   {  \n\
  pop \n\
0 0 5 1 rectfill\n\
6 0 2 1 rectfill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P15 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 1 9]\n\
/XStep 1\n\
/YStep 9\n\
/PaintProc\n\
   {  \n\
  pop \n\
0 0 1 5 rectfill\n\
0 6 1 2 rectfill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P16 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 8 5]\n\
/XStep 8\n\
/YStep 5\n\
/mainpat {\n\
0 1 4 1.5 rectfill\n\
5 0 2 4 rectfill\n\
} bind def\n\
/PaintProc\n\
   {  \n\
  pop \n\
mainpat\n\
8 0 translate\n\
mainpat\n\
-16 0 translate\n\
mainpat\n\
0 5 translate\n\
mainpat\n\
8 0 translate\n\
mainpat\n\
8 0 translate\n\
mainpat\n\
0 -10 translate\n\
mainpat\n\
-8 0 translate\n\
mainpat\n\
-8 0 translate\n\
mainpat\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P18 exch def\n\
<< /PatternType 1\n\
/PaintType 2\n\
/TilingType 1\n\
/BBox [0 0 9 10]\n\
/XStep 9\n\
/YStep 10\n\
/PaintProc\n\
   {  \n\
  pop \n\
0 0 8 4 rectfill\n\
-4 5 8 4 rectfill\n\
5 5 8 4 rectfill\n\
}bind\n\
\n\
>>\n\
matrix\n\
makepattern\n\
/P20 exch def\n\
"
