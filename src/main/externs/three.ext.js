/**
 * @fileoverview Closure Compiler externs for the Three.js r128 CDN global.
 * @externs
 */

/**
 * The Three.js global object provided by three@0.128.0/build/three.min.js.
 *
 * @type {!Object}
 * @const
 * @suppress {const|duplicate}
 */
var THREE = {};

/**
 * The Three.js release revision.
 *
 * @type {string}
 * @const
 */
THREE.REVISION;

/** @constructor */
THREE.Scene = function() {};

/**
 * @param {!Object} object
 * @return {void}
 */
THREE.Scene.prototype.add = function(object) {};

/**
 * @constructor
 * @param {number} fov
 * @param {number} aspect
 * @param {number} near
 * @param {number} far
 */
THREE.PerspectiveCamera = function(fov, aspect, near, far) {};

/** @type {!Object} */
THREE.PerspectiveCamera.prototype.position;

/** @type {number} */
THREE.PerspectiveCamera.prototype.aspect;

/**
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @return {void}
 */
THREE.PerspectiveCamera.prototype.lookAt = function(x, y, z) {};

/** @return {void} */
THREE.PerspectiveCamera.prototype.updateProjectionMatrix = function() {};

/**
 * @constructor
 * @param {!Object=} parameters
 */
THREE.WebGLRenderer = function(parameters) {};

/** @type {!Element} */
THREE.WebGLRenderer.prototype.domElement;

/**
 * @param {number} value
 * @return {void}
 */
THREE.WebGLRenderer.prototype.setPixelRatio = function(value) {};

/**
 * @param {number} width
 * @param {number} height
 * @param {boolean=} updateStyle
 * @return {void}
 */
THREE.WebGLRenderer.prototype.setSize = function(width, height, updateStyle) {};

/**
 * @param {number} color
 * @param {number=} alpha
 * @return {void}
 */
THREE.WebGLRenderer.prototype.setClearColor = function(color, alpha) {};

/**
 * @param {!THREE.Scene} scene
 * @param {!THREE.PerspectiveCamera} camera
 * @return {void}
 */
THREE.WebGLRenderer.prototype.render = function(scene, camera) {};

/** @return {void} */
THREE.WebGLRenderer.prototype.dispose = function() {};

/**
 * @constructor
 * @param {number} width
 * @param {number} height
 */
THREE.PlaneGeometry = function(width, height) {};

/** @return {void} */
THREE.PlaneGeometry.prototype.dispose = function() {};

/**
 * @constructor
 * @param {!Object=} parameters
 */
THREE.MeshBasicMaterial = function(parameters) {};

/** @type {?THREE.Texture} */
THREE.MeshBasicMaterial.prototype.map;

/** @type {!THREE.Color} */
THREE.MeshBasicMaterial.prototype.color;

/** @type {boolean} */
THREE.MeshBasicMaterial.prototype.needsUpdate;

/** @return {void} */
THREE.MeshBasicMaterial.prototype.dispose = function() {};

/**
 * @constructor
 * @param {!THREE.PlaneGeometry} geometry
 * @param {!THREE.MeshBasicMaterial} material
 */
THREE.Mesh = function(geometry, material) {};

/** @type {!THREE.Vector3} */
THREE.Mesh.prototype.position;

/** @type {!THREE.Euler} */
THREE.Mesh.prototype.rotation;

/** @constructor */
THREE.TextureLoader = function() {};

/**
 * @param {string} url
 * @param {function(!THREE.Texture)=} onLoad
 * @param {function(*):void=} onProgress
 * @param {function(*):void=} onError
 * @return {!THREE.Texture}
 */
THREE.TextureLoader.prototype.load = function(url, onLoad, onProgress, onError) {};

/** @constructor */
THREE.Texture = function() {};

/** @type {!HTMLImageElement} */
THREE.Texture.prototype.image;

/** @type {!THREE.Vector2} */
THREE.Texture.prototype.repeat;

/** @type {!THREE.Vector2} */
THREE.Texture.prototype.offset;

/** @type {*} */
THREE.Texture.prototype.minFilter;

/** @type {*} */
THREE.Texture.prototype.magFilter;

/** @type {boolean} */
THREE.Texture.prototype.needsUpdate;

/** @return {void} */
THREE.Texture.prototype.dispose = function() {};

/** @constructor */
THREE.Vector2 = function() {};

/**
 * @param {number} x
 * @param {number} y
 * @return {void}
 */
THREE.Vector2.prototype.set = function(x, y) {};

/** @constructor */
THREE.Vector3 = function() {};

/** @type {number} */
THREE.Vector3.prototype.z;

/**
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @return {void}
 */
THREE.Vector3.prototype.set = function(x, y, z) {};

/** @constructor */
THREE.Euler = function() {};

/** @type {number} */
THREE.Euler.prototype.z;

/** @constructor */
THREE.Color = function() {};

/**
 * @param {number} color
 * @return {void}
 */
THREE.Color.prototype.set = function(color) {};

/** @const {*} */
THREE.DoubleSide;

/** @const {*} */
THREE.LinearFilter;
