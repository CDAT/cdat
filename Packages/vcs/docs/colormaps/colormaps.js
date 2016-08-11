var canvas;
var gl;
var squareVerticesBuffer;
var textureCoordinatesBuffer;
var mvMatrix;
var shaderProgram;
var vertexPositionAttribute;
var textureCoordinateAttribute;
var perspectiveMatrix;

var windowWidth = 1200;
var windowHeight = 800;
var count;
var vertOffset;
var width;
var height;
var colors = {}
var texture;
var textures = {};
var lastFormat;

function createTexture(gl, data) {
    var textureFormat;

    if (data.length === 1024) {
      textureFormat = gl.RGBA;
    } else if (data.length == 768) {
      textureFormat = gl.RGB;
    } else {
      console.error("Invalid texture format");
    }

    var data = new Uint8Array(data);
    if (!texture || lastFormat !== textureFormat) {
      console.log("creating texture");
      if (texture) {
        gl.deleteTexture(texture);
      }
      texture = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texImage2D(gl.TEXTURE_2D, 0, textureFormat, 256, 1, 0, textureFormat, gl.UNSIGNED_BYTE, data);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    } else {
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texSubImage2D(
        gl.TEXTURE_2D, 0, 0, 0, 256, 1, textureFormat,
        gl.UNSIGNED_BYTE, data);
    }
    lastFormat = textureFormat;
    gl.bindTexture(gl.TEXTURE_2D, null);
    return texture;
}

//
// start
//
// Called when the canvas is created to get the ball rolling.
// Figuratively, that is. There's nothing moving in this demo.
//
// Expects initial.attributes file to be available at the same location as
// the script
function start() {

  $.getJSON( "initial.attributes", function( data ) {
    var items = [];
    // $.each( data, function( key, val ) {
    //   console.log(key, val);
    // });
    var cp = data['Cp'];
    var i = 0;
    $.each( cp, function(key, val) {
      var rawColorData = [];
      var colorName = key;
      var colorData = val['index']['data'];
      var sortedColorData = Object.keys(colorData).sort().reduce((r, k) => (r[k] = colorData[k], r), {});
      var rgba = (val.index.data[0].length === 4) || false;
      $.each( sortedColorData, function(key, val) {
          rawColorData = rawColorData.concat(val.map(x => Math.floor(x * 0.01 * 255)));
      });

      colors[colorName] = rawColorData;
    });

    canvas = document.getElementById("glcanvas");

    initWebGL(canvas);      // Initialize the GL context

    // Only continue if WebGL is available and working

    if (gl) {
      gl.clearColor(1.0, 1.0, 1.0, 1.0);  // Clear to black, fully opaque
      gl.clearDepth(1.0);                 // Clear everything
      gl.enable(gl.DEPTH_TEST);           // Enable depth testing
      gl.depthFunc(gl.LEQUAL);            // Near things obscure far things

      // Initialize the shaders; this is where all the lighting for the
      // vertices and so forth is established.

      initShaders();

      // Here's where we call the routine that builds all the objects
      // we'll be drawing.
      initTextures();

      initBuffers();

      // Draw now
      drawScene();
    }
  });
}



//
// initWebGL
//
// Initialize WebGL, returning the GL context or null if
// WebGL isn't available or could not be initialized.
//
function initWebGL() {
  gl = null;

  try {
    gl = canvas.getContext("webgl");
  }
  catch(e) {
  }

  // If we don't have a GL context, give up now

  if (!gl) {
    alert("Unable to initialize WebGL. Your browser may not support it.");
  }
}

//
// initBuffers
//
// Initialize the buffers we'll need. For this demo, we just have
// one object -- a simple two-dimensional square.
//
function initBuffers() {

  // Create a buffer for the square's vertices.

  squareVerticesBuffer = gl.createBuffer();

  // Select the squareVerticesBuffer as the one to apply vertex
  // operations to from here out.

  gl.bindBuffer(gl.ARRAY_BUFFER, squareVerticesBuffer);

  // Now create an array of vertices for the square. Note that the Z
  // coordinate is always 0 here.

  var vertices = [
    width,  height,  0.0,
    0, height,  0.0,
    width,  0.0, 0.0,
    0, 0.0, 0.0
  ];


  // Now pass the list of vertices into WebGL to build the shape. We
  // do this by creating a Float32Array from the JavaScript array,
  // then use it to fill the current vertex buffer.

  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);

  // Now set up the textureCoordiantes for the vertices

  var textureCoordiantes = [
    1.0, 1.0,
    0.0, 1.0,
    1.0, 0.0,
    0.0, 0.0
  ];

  textureCoordinatesBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, textureCoordinatesBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(textureCoordiantes), gl.STATIC_DRAW);
}

function initTextures() {
  var i = 0;

  count = Object.keys(colors).length;
  vertOffset = 2.0;
  horiOffset = windowWidth * 0.14;
  width = windowWidth * 0.86;
  height = (windowHeight/count) - (((count -1) * vertOffset)/count);
}

//
// drawScene
//
// Draw the scene.
//
function drawScene() {
  // Clear the canvas before we start drawing on it.

  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

  // Establish the perspective with which we want to view the
  // scene. Our field of view is 45 degrees, with a width/height
  // ratio of 640:480, and we only want to see objects between 0.1 units
  // and 100 units away from the camera.

  // perspectiveMatrix = makePerspective(45, 1920/1080, 0.1, 100.0);

  perspectiveMatrix = makeOrtho(0, windowWidth, 0, windowHeight, 0.01, 100.0);

  // Set the drawing position to the "identity" point, which is
  // the center of the scene.

  loadIdentity();

  // Now move the drawing position a bit to where we want to start
  // drawing the square.

  var i = 0;
  mvTranslate([horiOffset, 0.0, 0.0]);

  $.each(colors, function(key, data) {

    gl.useProgram(shaderProgram);

    if (i !== 0) {
      mvTranslate([0, (height + vertOffset), -1.0]);
    } else {
      mvTranslate([0, 0.0, -1.0]);
    }

    texture = createTexture(gl, data);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.uniform1i(gl.getUniformLocation(shaderProgram, "uSampler"), 0);

    // Draw the square by binding the array buffer to the square's vertices
    // array, setting attributes, and pushing it to GL.

    gl.bindBuffer(gl.ARRAY_BUFFER, squareVerticesBuffer);
    gl.vertexAttribPointer(vertexPositionAttribute, 3, gl.FLOAT, false, 0, 0);

    // Set the textureCoordiantes attribute for the vertices.

    gl.bindBuffer(gl.ARRAY_BUFFER, textureCoordinatesBuffer);
    gl.vertexAttribPointer(textureCoordinateAttribute, 2, gl.FLOAT, false, 0, 0);

    // Draw the square.
    setMatrixUniforms();
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

    var top = windowHeight - i * height - height * 0.5 - 15 - vertOffset * i;

    $('body').append('<p style="position: absolute; left: 15px; top: ' + top + 'px">'+key+'<p>');
    i += 1;

    gl.bindTexture(gl.TEXTURE_2D, null);
    gl.useProgram(null);
  });
}

//
// initShaders
//
// Initialize the shaders, so WebGL knows how to light our scene.
//
function initShaders() {
  var fragmentShader = getShader(gl, "shader-fs");
  var vertexShader = getShader(gl, "shader-vs");

  // Create the shader program

  shaderProgram = gl.createProgram();
  gl.attachShader(shaderProgram, vertexShader);
  gl.attachShader(shaderProgram, fragmentShader);
  gl.linkProgram(shaderProgram);

  // If creating the shader program failed, alert

  if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
    alert("Unable to initialize the shader program: " + gl.getProgramInfoLog(shader));
  }

  gl.useProgram(shaderProgram);

  vertexPositionAttribute = gl.getAttribLocation(shaderProgram, "aVertexPosition");
  gl.enableVertexAttribArray(vertexPositionAttribute);

  textureCoordinateAttribute = gl.getAttribLocation(shaderProgram, "aTextureCoordinate");
  gl.enableVertexAttribArray(textureCoordinateAttribute);
}

//
// getShader
//
// Loads a shader program by scouring the current document,
// looking for a script with the specified ID.
//
function getShader(gl, id) {
  var shaderScript = document.getElementById(id);

  // Didn't find an element with the specified ID; abort.

  if (!shaderScript) {
    return null;
  }

  // Walk through the source element's children, building the
  // shader source string.

  var theSource = "";
  var currentChild = shaderScript.firstChild;

  while(currentChild) {
    if (currentChild.nodeType == 3) {
      theSource += currentChild.textContent;
    }

    currentChild = currentChild.nextSibling;
  }

  // Now figure out what type of shader script we have,
  // based on its MIME type.

  var shader;

  if (shaderScript.type == "x-shader/x-fragment") {
    shader = gl.createShader(gl.FRAGMENT_SHADER);
  } else if (shaderScript.type == "x-shader/x-vertex") {
    shader = gl.createShader(gl.VERTEX_SHADER);
  } else {
    return null;  // Unknown shader type
  }

  // Send the source to the shader object

  gl.shaderSource(shader, theSource);

  // Compile the shader program

  gl.compileShader(shader);

  // See if it compiled successfully

  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    alert("An error occurred compiling the shaders: " + gl.getShaderInfoLog(shader));
    return null;
  }

  return shader;
}

//
// Matrix utility functions
//

function loadIdentity() {
  mvMatrix = Matrix.I(4);
}

function multMatrix(m) {
  mvMatrix = mvMatrix.x(m);
}

function mvTranslate(v) {
  multMatrix(Matrix.Translation($V([v[0], v[1], v[2]])).ensure4x4());
}

function setMatrixUniforms() {
  var pUniform = gl.getUniformLocation(shaderProgram, "uPMatrix");
  gl.uniformMatrix4fv(pUniform, false, new Float32Array(perspectiveMatrix.flatten()));

  var mvUniform = gl.getUniformLocation(shaderProgram, "uMVMatrix");
  gl.uniformMatrix4fv(mvUniform, false, new Float32Array(mvMatrix.flatten()));
}
