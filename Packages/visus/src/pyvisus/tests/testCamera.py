"""Test simple functions (i.e. no pointers involved)"""
from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *

from pyvisus.core import VisusFont, VisusCamera, VisusXMLInterface, VisusOpenGLState
from pyvisus.component import VisusSphereNode, VisusSceneNode, VisusLabelNode


window = None
gFocus = None
gRoot = None
winWidth = 800
winHeight= 600
gMouseX = 0
gMouseY = 0
gPressed = 0
gMouseMotion = False
gT = 0
gModifiers = 0

def init():
    global winWidth, winHeight

    light_ambient = [ 1, 1, 1, 1]
    light_diffuse = [ 1, 1, 1, 1]
    light_specular= [ 1, 1, 1, 1]
    light_position= [ 1, 1, 1, 0]

    glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient)
    glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse)
    glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular)
    glLightfv(GL_LIGHT0, GL_POSITION, light_position)

    glEnable(GL_LIGHTING)

    glMatrixMode( GL_PROJECTION )
    glLoadIdentity()
    gluLookAt(0,0,10,0,0,0,0,1,0)
    glOrtho(-10,10,-10*winHeight/float(winWidth),10*winHeight/float(winWidth),-100,100)

    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    return


def display():
    state = VisusOpenGLState()
    state.fetchState()
    gRoot.setValue(state)
  
    glutSetWindow(window);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glColor3f(1,1,1)
    glLineWidth(1.0)
    gRoot.display()
    glFlush ()
    glutSwapBuffers()
    return


def reshape( *args ):
    (x, y) = args
    glutSetWindow(window);
    glViewport(0,0,x,y)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-10,10,-10*3/4.0,10*3/4.0,-100,100)
    gluLookAt(0,0,10,0,0,-1,0,1,0)
    return


def motion( *args ):
    global gFocus,gPressed,gMouseMotion,gMouseX,gMouseY,gSceneFile,gModifiers
    (x, y) = args 

    if not gMouseMotion:
        return

    newX = (x-gMouseX) / (1.0 * winWidth)
    newY = (y-gMouseY) / (1.0 * winHeight)

    if gModifiers and GLUT_ACTIVE_SHIFT:
      if gPressed == GLUT_LEFT_BUTTON:
        gRoot.scaleJoystick(newX,newY)
      elif gPressed == GLUT_MIDDLE_BUTTON:
        gRoot.translateJoystick(newX,newY)
      elif gPressed == GLUT_RIGHT_BUTTON:
        gRoot.rotateJoystick(newX,newY)
    else:
      if gPressed == GLUT_LEFT_BUTTON:
        gRoot.scale(newX,newY)
      elif gPressed == GLUT_MIDDLE_BUTTON:
        gRoot.translate(newX,newY)
      elif gPressed == GLUT_RIGHT_BUTTON:
        gRoot.rotate(newX,newY)

    gMouseX = x
    gMouseY = y

    glutPostRedisplay()
    return


def mouse( *args ):
    global gPressed,gMouseMotion,gMouseX,gMouseY,gModifiers
    (button, state, x, y) = args

    if state == GLUT_DOWN:
      gModifiers = glutGetModifiers()
      gPressed = button
      gMouseMotion = True
      gMouseX = x
      gMouseY = y
    else:
      gModifiers = 0
      gMouseMotion = False
      gPressed = -1

    glutPostRedisplay()
    return


def keyboard( *args ):
    global gT
    (key, x, y) = args

    if key == 27:
      sys.exit(0)
    elif key == 'm':
      VisusXMLInterface().write("restart.xml", gRoot)
    elif key == 's':
      camera = VisusCamera()
      gRoot.getValue(camera)
      camera.save("camera.txt")
    elif key == 'l':
      gT = 0
      camera = VisusCamera()
      camera.load("camera.txt")
      gRoot.setValue(camera)
    elif key == 'i':
      camera1 = VisusCamera()
      camera1.load("origin.txt")
      camera2 = VisusCamera()
      camera2.load("camera.txt")
      gT += 0.1
      camera = VisusCamera.interpolate(camera1,camera2,gT)
      gRoot.setValue(camera)
    elif key == 'x':
      sys.exit(0)
    
    glutPostRedisplay()
    return

def idle():
    glutSetWindow(window)
    glutPostRedisplay()
    return


if __name__ == "__main__":
  import sys

  newArgv = glutInit(sys.argv)

  glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_MULTISAMPLE )
  glutInitWindowSize(winWidth, winHeight)
  glutInitWindowPosition(200, 200)
  window = glutCreateWindow("ViSUS Camera Test")

  glutDisplayFunc( display )
  glutReshapeFunc( reshape )
  glutMouseFunc( mouse )
  glutKeyboardFunc( keyboard )
  glutMotionFunc( motion )
  glutIdleFunc( idle )

  init()

  # Create the default scene graph 
  gRoot = VisusSceneNode.construct()

  # Create Sphere
  sphere = VisusSphereNode.construct()
  gRoot.attachSubTree(sphere)
  gFocus = sphere

  # Create Text
  font = VisusFont()
  font.fontSize(3);

  text = VisusLabelNode.construct();
  text.text("Visus camera test.. (s)ave (l)oad (i)nterpolate");
  text.setValue(font);
  text.position(-1,-0.9);
  gRoot.attachSubTree(text);
        
  text = VisusLabelNode.construct();
  text.text("Mouse click: left=scale middle=translate right=rotate");
  text.setValue(font);
  text.position(-1,-1);
  gRoot.attachSubTree(text);

  # Run The Main
  glutMainLoop()


