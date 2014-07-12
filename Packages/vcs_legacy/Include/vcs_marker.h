
/* Gmktype - additional VCS marker types */
/* The other Gmktype - marker types (listed below) are defined 
   in the gks.h file.
#define GMK_POINT       1
#define GMK_PLUS        2
#define GMK_STAR        3
#define GMK_O           4
#define GMK_X           5
*/

#define GMK_DIAMOND    		 6
#define GMK_TRIANGLEUP  	 7
#define GMK_TRIANGLEDOWN	 8
#define GMK_TRIANGLELEFT	 9
#define GMK_TRIANGLERIGHT	10
#define GMK_SQUARE     		11
#define GMK_DIAMOND_FILL	12
#define GMK_TRIANGLEUP_FILL  	13
#define GMK_TRIANGLEDOWN_FILL	14
#define GMK_TRIANGLELEFT_FILL	15
#define GMK_TRIANGLERIGHT_FILL	16
#define GMK_SQUARE_FILL		17
#define GMK_HURRICANE	        18
#define GMK_NONE		19

#define GMK_w00                  100
#define GMK_w01                  101
#define GMK_w02                  102
#define GMK_w03                  103
#define GMK_w04                  104
#define GMK_w05                  105
#define GMK_w06                  106
#define GMK_w07                  107
#define GMK_w08                  108
#define GMK_w09                  109
#define GMK_w10                  110
#define GMK_w11                  111
#define GMK_w12                  112
#define GMK_w13                  113
#define GMK_w14                  114
#define GMK_w15                  115
#define GMK_w16                  116
#define GMK_w17                  117
#define GMK_w18                  118
#define GMK_w19                  119
#define GMK_w20                  120
#define GMK_w21                  121
#define GMK_w22                  122
#define GMK_w23                  123
#define GMK_w24                  124
#define GMK_w25                  125
#define GMK_w26                  126
#define GMK_w27                  127
#define GMK_w28                  128
#define GMK_w29                  129
#define GMK_w30                  130
#define GMK_w31                  131
#define GMK_w32                  132
#define GMK_w33                  133
#define GMK_w34                  134
#define GMK_w35                  135
#define GMK_w36                  136
#define GMK_w37                  137
#define GMK_w38                  138
#define GMK_w39                  139
#define GMK_w40                  140
#define GMK_w41                  141
#define GMK_w42                  142
#define GMK_w43                  143
#define GMK_w44                  144
#define GMK_w45                  145
#define GMK_w46                  146
#define GMK_w47                  147
#define GMK_w48                  148
#define GMK_w49                  149
#define GMK_w50                  150
#define GMK_w51                  151
#define GMK_w52                  152
#define GMK_w53                  153
#define GMK_w54                  154
#define GMK_w55                  155
#define GMK_w56                  156
#define GMK_w57                  157
#define GMK_w58                  158
#define GMK_w59                  159
#define GMK_w60                  160
#define GMK_w61                  161
#define GMK_w62                  162
#define GMK_w63                  163
#define GMK_w64                  164
#define GMK_w65                  165
#define GMK_w66                  166
#define GMK_w67                  167
#define GMK_w68                  168
#define GMK_w69                  169
#define GMK_w70                  170
#define GMK_w71                  171
#define GMK_w72                  172
#define GMK_w73                  173
#define GMK_w74                  174
#define GMK_w75                  175
#define GMK_w76                  176
#define GMK_w77                  177
#define GMK_w78                  178
#define GMK_w79                  179
#define GMK_w80                  180
#define GMK_w81                  181
#define GMK_w82                  182
#define GMK_w83                  183
#define GMK_w84                  184
#define GMK_w85                  185
#define GMK_w86                  186
#define GMK_w87                  187
#define GMK_w88                  188
#define GMK_w89                  189
#define GMK_w90                  190
#define GMK_w91                  191
#define GMK_w92                  192
#define GMK_w93                  193
#define GMK_w94                  194
#define GMK_w95                  195
#define GMK_w96                  196
#define GMK_w97                  197
#define GMK_w98                  198
#define GMK_w99                  199
#define GMK_w200                 200
#define GMK_w201                 201
#define GMK_w202                 202


/*		A structure for VCS marker attributes.	*/
/*		Values are initialized in main.				*/

  struct vcs_legacy_marker
    {
     int type;			/* The marker type.			*/
     float size;		/* The size of the marker.		*/
     int colour;		/* The marker color.			*/
    };
