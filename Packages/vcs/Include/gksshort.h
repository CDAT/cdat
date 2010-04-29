

/*
#define    gopks     gopengks
#define    gclks     gclosegks
#define    gopwk     gopenws
#define    gclwk     gclosews
#define    gacwk     gactivatews
#define    gdacwk    gdeactivatews
#define    gclrwk    gclearws
#define    grsgwk    gredrawsegsws
#define    guwk      gupdatews
#define    gsds      gsetdeferst
#define    gmsg      gmessage
#define    gpl       gpolyline
#define    gpm       gpolymarker
#define    gtx       gtext
#define    gfa       gfillarea
#define    gca       gcellarray
#define    ggdp      ggdp
#define    gspli     gsetlineind
#define    gsln      gsetlinetype
#define    gslwsc    gsetlinewidth
#define    gsplci    gsetlinecolorind
#define    gspmi     gsetmarkerind
#define    gsmk      gsetmarkertype
#define    gsmksc    gsetmarkersize
#define    gspmci    gsetmarkercolorind
#define    gstxi     gsettextind
#define    gstxfp    gsettextfontprec
#define    gschxp    gsetcharexpan
#define    gschsp    gsetcharspace
#define    gstxci    gsettextcolorind
#define    gschh     gsetcharheight
#define    gschup    gsetcharup
#define    gstxp     gsettextpath
#define    gstxal    gsettextalign
#define    gsfai     gsetfillind
#define    gsfais    gsetfillintstyle
#define    gsfasi    gsetfillstyleind
#define    gsfaci    gsetfillcolorind
#define    gspa      gsetpatsize
#define    gsparf    gsetpatrefpt
#define    gsasf     gsetasf
#define    gspkid    gsetpickid
#define    gsplr     gsetlinerep
#define    gspmr     gsetmarkerrep
#define    gstxr     gsettextrep
#define    gsfar     gsetfillrep
#define    gspar     gsetpatrep
#define    gscr      gsetcolorrep
#define    gswn      gsetwindow
#define    gsvp      gsetviewport
#define    gsvpip    gsetviewportinputpri
#define    gselnt    gselntran
#define    gsclip    gsetclip
#define    gswkwn    gsetwswindow
#define    gswkvp    gsetwsviewport
#define    gcrsg     gcreateseg
#define    gclsg     gcloseseg
#define    grensg    grenameseg
#define    gdsg      gdelseg
#define    gdsgwk    gdelsegws
#define    gasgwk    gassocsegws
#define    gcsgwk    gcopysegws
#define    ginsg     ginsertseg
#define    gssgt     gsetsegtran
#define    gsvis     gsetvis
#define    gshlit    gsethighl
#define    gssgp     gsetsegpri
#define    gsdtec    gsetdet
#define    ginlc     ginitloc
#define    ginsk     ginitstroke
#define    ginvl     ginitval
#define    ginch     ginitchoice
#define    ginpk     ginitpick
#define    ginst     ginitstring
#define    gslcm     gsetlocmode
#define    gsskm     gsetstrokemode
#define    gsvlm     gsetvalmode
#define    gschm     gsetchoicemode
#define    gspkm     gsetpickmode
#define    gsstm     gsetstringmode
#define    grqlc     greqloc
#define    grqsk     greqstroke
#define    grqvl     greqval
#define    grqch     greqchoice
#define    grqpk     greqpick
#define    grqst     greqstring
#define    gsmlc     gsampleloc
#define    gsmsk     gsamplestroke
#define    gsmvl     gsampleval
#define    gsmch     gsamplechoice
#define    gsmpk     gsamplepick
#define    gsmst     gsamplestring
#define    gwait     gawaitevent
#define    gflush    gflushevents
#define    ggtlc     ggetloc
#define    ggtsk     ggetstroke
#define    ggtvl     ggetval
#define    ggtch     ggetchoice
#define    ggtpk     ggetpick
#define    ggtst     ggetstring
#define    gwitm     gwritegksm
#define    ggtitm    ggetgksm
#define    grditm    greadgksm
#define    giitm     ginterpret
#define    gqops     ginqopst
#define    gqlvks    ginqlevelgks
#define    gqavwk    ginqavailwstypes
#define    gqwkm     ginqwsmaxnum
#define    gqmntn    ginqmaxntrannum
#define    gqopwk    ginqopenws
#define    gqacwk    ginqactivews
#define    gqpli     ginqlineindices
#define    gqpmi     ginqmarkerindices
#define    gqtxi     ginqtextindices
#define    gqchh     ginqcharht
#define    gqchw     ginqcharwidth
#define    gqchup    ginqcharupvec
#define    gqchb     ginqcharbasevec
#define    gqtxp     ginqtextpath
#define    gqtxal    ginqtextalign
#define    gqfai     ginqfillindices
#define    gqparf    ginqpatrefpoint
#define    gqpkid    ginqcurpickid
#define    gqln      ginqlinetype
#define    gqlwsc    ginqlinewidth
#define    gqplci    ginqlinecolrind
#define    gqmk      ginqmarkertype
#define    gqmksc    ginqmarkersize
#define    gqpmci    ginqmarkercolrind
#define    gqtxfp    ginqtextfontprec
#define    gqchxp    ginqcharexpan
#define    gqchsp    ginqcharspace
#define    gqtxci    ginqtextcolrind
#define    gqfais    ginqfillintstyle
#define    gqfasi    ginqfillstyleind
#define    gqfaci    ginqfillcolrind
#define    gqasf     ginqasf
#define    gqcntn    ginqcurntrannum
#define    gqentn    ginqntrannum
#define    gqclip    ginqclip
#define    gqopsg    ginqnameopenseg
#define    gqsgus    ginqsegnames
#define    gqsim     ginqmoreevents
#define    gqwkc     ginqwsconntype
#define    gqwks     ginqwsst
#define    gqwkdu    ginqwsdeferupdatest
#define    gqepli    ginqlineinds
#define    gqplr     ginqlinerep
#define    gqepmi    ginqlistmarkerinds
#define    gqpmr     ginqmarkerrep
#define    gqtxr     ginqtextrep
#define    gqtxx     ginqtextextent
#define    gqfar     ginqfillrep
#define    gqepai    ginqpatindices
#define    gqpar     ginqpatrep
#define    gqeci     ginqcolorindices
#define    gqcr      ginqcolorrep
#define    gqwkt     ginqwstran
#define    gqsgwk    ginqsegnamesws
#define    gqlcs     ginqlocst
#define    gqsks     ginqstrokest
#define    gqvls     ginqvalst
#define    gqchs     ginqchoicest
#define    gqpks     ginqpickst
#define    gqsts     ginqstringst
#define    gqwkca    ginqwscategory
#define    gqwkcl    ginqwsclass
#define    gqdsp     ginqdisplayspacesize
#define    gqdwka    ginqmodwsattr
#define    gqdds     ginqdefdeferst
#define    gqplf     ginqlinefacs
#define    gqpplr    ginqpredlinerep
#define    gqpmf     ginqmarkerfacil
#define    gqppmr    ginqpredmarkerrep
#define    gqtxf     ginqtextfacil
#define    gqptxr    ginqpredtextrep
#define    gqfaf     ginqfillfacil
#define    gqpfar    ginqpredfillrep
#define    gqpaf     ginqpatfacil
#define    gqpa      ginqpatsize
#define    gqppar    ginqpredpatrep
#define    gqcf      ginqcolorfacil
#define    gqpcr     ginqpredcolorrep
#define    gqegdp    ginqavailgdps
#define    gqgdp     ginqgdp
#define    gqlwk     ginqmaxwssttables
#define    gqsgp     ginqnumsegpri
#define    gqdsga    ginqmodsegattr
#define    gqli      ginqnumavailinput
#define    gqdlc     ginqdefloc
#define    gqdsk     ginqdefstroke
#define    gqdvl     ginqdefval
#define    gqdch     ginqdefchoice
#define    gqdpk     ginqdefpick
#define    gqdst     ginqdefstring
#define    gqaswk    ginqassocws

#define    gqpxad    ginqpixelarraydim
#define    gqpxa     ginqpixelarray
#define    gqpx      ginqpixel

#define    gqiqov    ginqinputoverflow

#define    gevtm     gevaltran
#define    gactm     gaccumtran
#define    geclks    gemergencyclosegks

#define    gqnt      ginqntran

#define    gssga     gsetsegattr

#define    gqpav     ginqprimattr
#define    gqsga     ginqsegattr
*/








#define    gactm     gaccumtran
#define    gacwk     gactivatews
#define    gasgwk    gassocsegws
#define    gwait     gawaitevent
#define    gca       gcellarray
#define    gclrwk    gclearws
#define    gclks     gclosegks
#define    gclsg     gcloseseg
#define    gclwk     gclosews
#define    gcsgwk    gcopysegws
#define    gcrsg     gcreateseg
#define    gdacwk    gdeactivatews
#define    gdsg      gdelseg
#define    gdsgwk    gdelsegws
#define    geclks    gemergencyclosegks

#define    gerhnd    gerrorhand
#define	   gerlog    gerrorlog

/*		Escape functions peculiar to XGKS.

		    gescinqxattr
		    gescredrawnotify
		    gescsetcolormask
		    gescsetdcsize
		    gescstoreprimi
*/

#define    gevtm     gevaltran
#define    gfa       gfillarea
#define    gflush    gflushevents
#define    ggtch     ggetchoice
#define    ggtitm    ggetgksm
#define    ggtlc     ggetloc
#define    ggtpk     ggetpick
#define    ggtsk     ggetstroke
#define    ggtvl     ggetval
#define    ginch     ginitchoice
#define    ginlc     ginitloc
#define    ginpk     ginitpick
#define    ginst     ginitstring
#define    ginsk     ginitstroke
#define    ginvl     ginitval
#define    gopwk     gopenws
#define    gqacwk    ginqactivews
#define    gqaswk    ginqassocws
#define    gqavwk    ginqavailwstypes

#define    gqchh     ginqcharht		/*	Not used in this XGKS?	*/
#define    gqchw     ginqcharwidth	/*	Not used in this XGKS?	*/
#define    gqchup    ginqcharupvec	/*	Not used in this XGKS?	*/
#define    gqchb     ginqcharbasevec	/*	Not used in this XGKS?	*/
#define    gqtxp     ginqtextpath	/*	Not used in this XGKS?	*/
#define    gqtxal    ginqtextalign	/*	Not used in this XGKS?	*/

#define    gqchs     ginqchoicest
#define    gqclip    ginqclip
#define    gqcf      ginqcolorfacil
#define    gqeci     ginqcolorindices
#define    gqcr      ginqcolorrep
#define    gqcntn    ginqcurntrannum
#define    gqpkid    ginqcurpickid
#define    gqdch     ginqdefchoice
#define    gqdds     ginqdefdeferst
#define    gqdlc     ginqdefloc
#define    gqdpk     ginqdefpick
#define    gqdst     ginqdefstring
#define    gqdsk     ginqdefstroke
#define    gqdvl     ginqdefval
#define    gqdsp     ginqdisplayspacesize
#define    gqfaf     ginqfillfacil
#define    gqfai     ginqfillindices
#define    gqfar     ginqfillrep

#define    gqegdp    ginqavailgdps/*	Not used in this XGKS?	*/
#define    gqgdp     ginqgdp	/*	Not used in this XGKS?	*/

#define    gqiqov    ginqinputoverflow
#define    gqlvks    ginqlevelgks
#define    gqplf     ginqlinefacs
#define    gqpli     ginqlineindices
#define    gqepli    ginqlistlineindices/*	Not used in this XGKS?	*/
#define    gqplr     ginqlinerep
#define    gqlcs     ginqlocst
#define    gqpmf     ginqmarkerfacil
#define    gqpmi     ginqmarkerindices
#define    gqepmi    ginqlistmarkerinds/*	Not used in this XGKS?	*/
#define    gqpmr     ginqmarkerrep
#define    gqmntn    ginqmaxntrannum
#define    gqlwk     ginqmaxwssttables
#define    gqdsga    ginqmodsegattr
#define    gqdwka    ginqmodwsattr
#define    gqsim     ginqmoreevents
#define    gqopsg    ginqnameopenseg
#define    gqnt      ginqntran
#define    gqentn    ginqntrannum
#define    gqli      ginqnumavailinput
#define    gqsgp     ginqnumsegpri
#define    gqopwk    ginqopenws
#define    gqops     ginqopst
#define    gqpaf     ginqpatfacil
#define    gqepai    ginqpatindices

#define    gqparf    ginqpatrefpoint/*	Not used by this XGKS?		*/

#define    gqpar     ginqpatrep

#define    gqpa      ginqpatsize/*	Not used by this XGKS?		*/

#define    gqpks     ginqpickst
#define    gqpx      ginqpixel
#define    gqpxa     ginqpixelarray
#define    gqpxad    ginqpixelarraydim
#define    gqpcr     ginqpredcolorrep
#define    gqpfar    ginqpredfillrep
#define    gqpplr    ginqpredlinerep
#define    gqppmr    ginqpredmarkerrep
#define    gqppar    ginqpredpatrep
#define    gqptxr    ginqpredtextrep
#define    gqpav     ginqprimattr
#define    gqsga     ginqsegattr
#define    gqsgus    ginqsegnames
#define    gqsgwk    ginqsegnamesws
#define    gqsts     ginqstringst
#define    gqsks     ginqstrokest
#define    gqtxx     ginqtextextent
#define    gqtxf     ginqtextfacil
#define    gqtxi     ginqtextindices
#define    gqtxr     ginqtextrep
#define    gqvls     ginqvalst
#define    gqwkca    ginqwscategory
#define    gqwkcl    ginqwsclass
#define    gqwkc     ginqwsconntype
#define    gqwkdu    ginqwsdeferupdatest
#define    gqwkm     ginqwsmaxnum
#define    gqwks     ginqwsst
#define    gqwkt     ginqwstran
#define    ginsg     ginsertseg
#define    giitm     ginterpret
#define    gmsg      gmessage
#define    gopks     gopengks
#define    gpl       gpolyline
#define    gpm       gpolymarker
#define    grditm    greadgksm
#define    grsgwk    gredrawsegsws
#define    grensg    grenameseg
#define    grqch     greqchoice
#define    grqlc     greqloc
#define    grqpk     greqpick
#define    grqst     greqstring
#define    grqsk     greqstroke
#define    grqvl     greqval
#define    gsmch     gsamplechoice
#define    gsmlc     gsampleloc
#define    gsmpk     gsamplepick
#define    gsmst     gsamplestring
#define    gsmsk     gsamplestroke
#define    gsmvl     gsampleval
#define    gselnt    gselntran
#define    gsasf     gsetasf
#define    gschxp    gsetcharexpan
#define    gschh     gsetcharheight
#define    gschsp    gsetcharspace
#define    gschup    gsetcharup
#define    gschm     gsetchoicemode
#define    gsclip    gsetclip
#define    gscr      gsetcolorrep
#define    gsds      gsetdeferst
#define    gsdtec    gsetdet/*	Not used for this xgks?		*/
#define    gsfaci    gsetfillcolorind
#define    gsfai     gsetfillind
#define    gsfais    gsetfillintstyle
#define    gsfar     gsetfillrep
#define    gsfasi    gsetfillstyleind
#define    gshlit    gsethighl/*	Not used for this xgks?		*/
#define    gsplci    gsetlinecolorind
#define    gspli     gsetlineind
#define    gsplr     gsetlinerep
#define    gsln      gsetlinetype
#define    gslwsc    gsetlinewidth
#define    gslcm     gsetlocmode
#define    gspmci    gsetmarkercolorind
#define    gspmi     gsetmarkerind
#define    gspmr     gsetmarkerrep
#define    gsmksc    gsetmarkersize
#define    gsmk      gsetmarkertype
#define    gsparf    gsetpatrefpt/*	Not used for this xgks.		*/
#define    gspar     gsetpatrep
#define    gspa      gsetpatsize/*	Not used for this xgks.		*/
#define    gspkid    gsetpickid
#define    gspkm     gsetpickmode
#define    gssgp     gsetsegpri /*	Not used for this xgks?		*/
#define    gssgt     gsetsegtran/*	Not used for this xgks?		*/
#define    gssga     gsetsegattr
#define    gsstm     gsetstringmode
#define    gsskm     gsetstrokemode
#define    gstxal    gsettextalign
#define    gstxci    gsettextcolorind
#define    gstxfp    gsettextfontprec
#define    gstxi     gsettextind
#define    gstxp     gsettextpath
#define    gstxr     gsettextrep
#define    gsvlm     gsetvalmode
#define    gsvp      gsetviewport
#define    gsvpip    gsetviewportinputpri
#define    gsvis     gsetvis/*	Not used for this xgks?		*/
#define    gswn      gsetwindow
#define    gswkvp    gsetwsviewport
#define    gswkwn    gsetwswindow
#define    gtx       gtext
#define    guwk      gupdatews
#define    gwitm     gwritegksm
