#ifndef VCS_EVENT_X11_MAPPING_H
/* mapping from X values to generic vcs_legacy values */
#define VCS_EVENT_X11_MAPPING_H

#define EVENT_TYPE event.type

#define VCS_ButtonPress ButtonPress
#define VCS_ButtonRelease ButtonRelease
#define VCS_CirculateNotify CirculateNotify
#define VCS_ConfigureNotify ConfigureNotify
#define VCS_CreateNotify CreateNotify
#define VCS_ClientMessage ClientMessage
#define VCS_DestroyNotify DestroyNotify
#define VCS_EnterNotify EnterNotify
#define VCS_LeaveNotify LeaveNotify
#define VCS_FocusIn FocusIn
#define VCS_FocusOut FocusOut
#define VCS_Expose Expose
#define VCS_GraphicsExpose GraphicsExpose
#define VCS_GravityNotify GravityNotify
#define VCS_KeyPress KeyPress
#define VCS_KeyRelease KeyRelease
#define VCS_MapNotify MapNotify
#define VCS_MotionNotify MotionNotify
#define VCS_NoExpose NoExpose
#define VCS_ReparentNotify ReparentNotify
#define VCS_UnmapNotify UnmapNotify

#define VCS_RESIZING event.xany.send_event != 0
#define BUTTON event.xbutton.button
#define BUTTON_X event.xbutton.x
#define BUTTON_Y event.xbutton.y
#define LEFT_BUTTON 1
#define MIDDLE_BUTTON 2
#define RIGHT_BUTTON 3
#define SCROOL_UP_BUTTON 4
#define SCROOL_DOWN_BUTTON 5

#define BACKSPACE_KEY XK_BackSpace
#define DELETE_KEY XK_Delete
#define RIGHT_KEY XK_Right
#define LEFT_KEY XK_Left
#define UP_KEY XK_Up
#define DOWN_KEY XK_Down

#define SHIFT_PRESSED event.xbutton.state == ShiftMask
/* 272 is a hack for the SuSE platform only. Can be removed in the future. */
#define LEFT_BUTTON_PRESSED_WHILE_MOVING (event.xmotion.state == Button1Mask) || (event.xmotion.state == 272)
#endif
