/*
 *   Header for simple GrADS GUI interface based on libsx.
 *
 *   --
 *   (c) 1997 by Arlindo da Silva
 *
 *   Permission is granted to any individual or institution to use,
 *   modify, copy, or redistribute this software so long as it is not 
 *   sold for profit, and provided this notice is retained. 
 *
 */
/* kk --- 020619 added List and Free_List --- kk */

#include "libsx.h"

int init_display(int argc, char **argv, void *data);
int Custom_GUI( char *fname );
int gagui_main(int argc, char **argv);

/* callback protos */
void CB_Exit       (Widget w, void *data);
void CB_CloseWindow(Widget w, void *data); 
void CB_Open       (Widget w, void *data);
void CB_Load       (Widget w, void *data);
void CB_Cmd        (Widget w, void *data);
void CB_CmdStr     (Widget w, void *data);
void CB_CmdLine    (Widget w, void *data);
void CB_Display    (Widget w, void *data);
void CB_Toggle     (Widget w, void *data);

void CB_VarSel     (Widget w, void *data);
void CB_VarOK      (Widget w, void *data);
void CB_VarCancel  (Widget w, void *data);
void CB_VarList    (Widget w, char *str, int index, void *data);
void CB_VarStr     (Widget w, char *str, int index, void *data);

void CB_CmdWin     (Widget w, void *data);
void CB_CmdWinOK   (Widget w, void *data);
void CB_CmdWinClear (Widget w, void *data);
void CB_CmdWinDone (Widget w, void *data);
void CB_CmdWinList (Widget w, char *str, int index, void *data);
void CB_CmdWinStr  (Widget w, char *str, int index, void *data);

void CB_FileSel    (Widget w, void *data);
void CB_Browse     (Widget w, void *data);
void CB_Edit       (Widget w, void *data);
void CB_FileList   (Widget w, char *str, int index, void *data);

/* kk --- 020619 added List and Free_List --- kk */
char **List(int rows,int cols);
void Free_List(char **list, int rows);
/* kk --- 020619 added List and Free_List --- kk */
typedef struct wininfo
{
  Widget window, text_widget, label_widget;
  int *num_windows;
  char *cur_path;
}WinInfo;
