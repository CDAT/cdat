#ifndef VCS_EVENT_QT_MAPPING_H
/* mapping from X values to generic vcs_legacy values */
#define VCS_EVENT_QT_MAPPING_H
#include <QtCore/QEvent>
#include <QtGui/QMouseEvent>

#define EVENT_TYPE event->type()

#define VCS_ButtonPress QEvent::Type(QEvent::MouseButtonPress)
#define VCS_ButtonRelease QEvent::Type(QEvent::MouseButtonRelease)
#define VCS_CirculateNotify VCS_UNDEFINED_EVENT_1
#define VCS_ConfigureNotify VCS_UNDEFINED_EVENT_2
#define VCS_CreateNotify VCS_UNDEFINED_EVENT_3
#define VCS_ClientMessage VCS_UNDEFINED_EVENT_4
#define VCS_DestroyNotify QEvent::Type(QEvent::Close)
#define VCS_EnterNotify QEvent::Type(QEvent::Enter)
#define VCS_LeaveNotify QEvent::Type(QEvent::Leave)
#define VCS_FocusIn QEvent::Type(QEvent::FocusIn)
#define VCS_FocusOut QEvent::Type(QEvent::FocusOut)
#define VCS_Expose VCS_UNDEFINED_EVENT_5
#define VCS_GraphicsExpose VCS_UNDEFINED_EVENT_6
#define VCS_GravityNotify VCS_UNDEFINED_EVENT_7
#define VCS_KeyPress QEvent::Type(QEvent::KeyPress)
#define VCS_KeyRelease QEvent::Type(QEvent::KeyRelease)
#define VCS_MapNotify QEvent::Type(QEvent::Show)
#define VCS_MotionNotify QEvent::Type(QEvent::Move)
#define VCS_NoExpose VCS_UNDEFINED_EVENT_8
#define VCS_ReparentNotify VCS_UNDEFINED_EVENT_9
#define VCS_UnmapNotify VCS_UNDEFINED_EVENT_10


#define VCS_RESIZING event->type()==QEvent::Type(QEvent::Resize)
#define BUTTON ((QMouseEvent *)event)->button()
#define BUTTON_X ((QMouseEvent *)event)->x()
#define BUTTON_Y ((QMouseEvent *)event)->y()
#define LEFT_BUTTON Qt::LeftButton
#define MIDDLE_BUTTON Qt::MidButton
#define RIGHT_BUTTON Qt::RightButton
#define SCROOL_UP_BUTTON Qt::XButton1
#define SCROOL_DOWN_BUTTON Qt::XButton2

#define BACKSPACE_KEY Qt::Key_Backspace
#define DELETE_KEY Qt::Key_Delete
#define RIGHT_KEY Qt::Key_Right
#define LEFT_KEY Qt::Key_Left
#define UP_KEY Qt::Key_Up
#define DOWN_KEY Qt::Key_Down

#define SHIFT_PRESSED -999 //event.xbutton.state == ShiftMask
#define LEFT_BUTTON_PRESSED_WHILE_MOVING -999
#endif
