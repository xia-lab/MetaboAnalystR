import { Group, Mesh, MeshLambertMaterial, Color, BufferGeometry, BufferAttribute, Matrix4, Vector3, SphereBufferGeometry, CylinderBufferGeometry, TubeBufferGeometry, ConeBufferGeometry, Line, LineBasicMaterial, QuadraticBezierCurve3, CubicBezierCurve3, Box3 } from "../three.module.js";
import { forceSimulation, forceLink, forceManyBody, forceCenter, forceRadial } from './d3-force-3d.js';

import * as temp from './ngraph.graph.js';
const graph = temp.default;
import Kapsule from './kapsule.module.js';
import accessorFn from './accessor-fn.module.js';
import { min, max } from './d3-array.js';

import dataJoint from './data-joint.module.js';
import { scaleOrdinal } from './d3-scale.js';
import { schemePaired } from './d3-scale-chromatic.js';
import tinyColor from './tinycolor-min.js';

function ownKeys(object, enumerableOnly) {
  var keys = Object.keys(object);

  if (Object.getOwnPropertySymbols) {
    var symbols = Object.getOwnPropertySymbols(object);
    enumerableOnly && (symbols = symbols.filter(function (sym) {
      return Object.getOwnPropertyDescriptor(object, sym).enumerable;
    })), keys.push.apply(keys, symbols);
  }

  return keys;
}

function _objectSpread2(target) {
  for (var i = 1; i < arguments.length; i++) {
    var source = null != arguments[i] ? arguments[i] : {};
    i % 2 ? ownKeys(Object(source), !0).forEach(function (key) {
      _defineProperty(target, key, source[key]);
    }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)) : ownKeys(Object(source)).forEach(function (key) {
      Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key));
    });
  }

  return target;
}

function _typeof(obj) {
  "@babel/helpers - typeof";

  return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) {
    return typeof obj;
  } : function (obj) {
    return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
  }, _typeof(obj);
}

function _classCallCheck(instance, Constructor) {
  if (!(instance instanceof Constructor)) {
    throw new TypeError("Cannot call a class as a function");
  }
}

function _defineProperties(target, props) {
  for (var i = 0; i < props.length; i++) {
    var descriptor = props[i];
    descriptor.enumerable = descriptor.enumerable || false;
    descriptor.configurable = true;
    if ("value" in descriptor) descriptor.writable = true;
    Object.defineProperty(target, descriptor.key, descriptor);
  }
}

function _createClass(Constructor, protoProps, staticProps) {
  if (protoProps) _defineProperties(Constructor.prototype, protoProps);
  if (staticProps) _defineProperties(Constructor, staticProps);
  Object.defineProperty(Constructor, "prototype", {
    writable: false
  });
  return Constructor;
}

function _defineProperty(obj, key, value) {
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }

  return obj;
}

function _inherits(subClass, superClass) {
  if (typeof superClass !== "function" && superClass !== null) {
    throw new TypeError("Super expression must either be null or a function");
  }

  subClass.prototype = Object.create(superClass && superClass.prototype, {
    constructor: {
      value: subClass,
      writable: true,
      configurable: true
    }
  });
  Object.defineProperty(subClass, "prototype", {
    writable: false
  });
  if (superClass) _setPrototypeOf(subClass, superClass);
}

function _getPrototypeOf(o) {
  _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) {
    return o.__proto__ || Object.getPrototypeOf(o);
  };
  return _getPrototypeOf(o);
}

function _setPrototypeOf(o, p) {
  _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) {
    o.__proto__ = p;
    return o;
  };

  return _setPrototypeOf(o, p);
}

function _isNativeReflectConstruct() {
  if (typeof Reflect === "undefined" || !Reflect.construct) return false;
  if (Reflect.construct.sham) return false;
  if (typeof Proxy === "function") return true;

  try {
    Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {}));
    return true;
  } catch (e) {
    return false;
  }
}

function _construct(Parent, args, Class) {
  if (_isNativeReflectConstruct()) {
    _construct = Reflect.construct;
  } else {
    _construct = function _construct(Parent, args, Class) {
      var a = [null];
      a.push.apply(a, args);
      var Constructor = Function.bind.apply(Parent, a);
      var instance = new Constructor();
      if (Class) _setPrototypeOf(instance, Class.prototype);
      return instance;
    };
  }

  return _construct.apply(null, arguments);
}

function _objectWithoutPropertiesLoose(source, excluded) {
  if (source == null) return {};
  var target = {};
  var sourceKeys = Object.keys(source);
  var key, i;

  for (i = 0; i < sourceKeys.length; i++) {
    key = sourceKeys[i];
    if (excluded.indexOf(key) >= 0) continue;
    target[key] = source[key];
  }

  return target;
}

function _objectWithoutProperties(source, excluded) {
  if (source == null) return {};

  var target = _objectWithoutPropertiesLoose(source, excluded);

  var key, i;

  if (Object.getOwnPropertySymbols) {
    var sourceSymbolKeys = Object.getOwnPropertySymbols(source);

    for (i = 0; i < sourceSymbolKeys.length; i++) {
      key = sourceSymbolKeys[i];
      if (excluded.indexOf(key) >= 0) continue;
      if (!Object.prototype.propertyIsEnumerable.call(source, key)) continue;
      target[key] = source[key];
    }
  }

  return target;
}

function _assertThisInitialized(self) {
  if (self === void 0) {
    throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
  }

  return self;
}

function _possibleConstructorReturn(self, call) {
  if (call && (typeof call === "object" || typeof call === "function")) {
    return call;
  } else if (call !== void 0) {
    throw new TypeError("Derived constructors may only return object or undefined");
  }

  return _assertThisInitialized(self);
}

function _createSuper(Derived) {
  var hasNativeReflectConstruct = _isNativeReflectConstruct();

  return function _createSuperInternal() {
    var Super = _getPrototypeOf(Derived),
        result;

    if (hasNativeReflectConstruct) {
      var NewTarget = _getPrototypeOf(this).constructor;

      result = Reflect.construct(Super, arguments, NewTarget);
    } else {
      result = Super.apply(this, arguments);
    }

    return _possibleConstructorReturn(this, result);
  };
}

function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
}

function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread();
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}

function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
}

function _iterableToArray(iter) {
  if (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null || iter["@@iterator"] != null) return Array.from(iter);
}

function _iterableToArrayLimit(arr, i) {
  var _i = arr == null ? null : typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"];

  if (_i == null) return;
  var _arr = [];
  var _n = true;
  var _d = false;

  var _s, _e;

  try {
    for (_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true) {
      _arr.push(_s.value);

      if (i && _arr.length === i) break;
    }
  } catch (err) {
    _d = true;
    _e = err;
  } finally {
    try {
      if (!_n && _i["return"] != null) _i["return"]();
    } finally {
      if (_d) throw _e;
    }
  }

  return _arr;
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;

  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];

  return arr2;
}

function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

var materialDispose = function materialDispose(material) {
  if (material instanceof Array) {
    material.forEach(materialDispose);
  } else {
    if (material.map) {
      material.map.dispose();
    }

    material.dispose();
  }
};

var deallocate = function deallocate(obj) {
  if (obj.geometry) {
    obj.geometry.dispose();
  }

  if (obj.material) {
    materialDispose(obj.material);
  }

  if (obj.texture) {
    obj.texture.dispose();
  }

  if (obj.children) {
    obj.children.forEach(deallocate);
  }
};

var emptyObject = function emptyObject(obj) {
  while (obj.children.length) {
    var childObj = obj.children[0];
    obj.remove(childObj);
    deallocate(childObj);
  }
};

var _excluded = ["objFilter"];

function threeDigest(data, scene) {
  var _ref = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};

  var _ref$objFilter = _ref.objFilter,
      objFilter = _ref$objFilter === void 0 ? function () {
    return true;
  } : _ref$objFilter,
      options = _objectWithoutProperties(_ref, _excluded);

  return dataJoint(data, scene.children.filter(objFilter), function (obj) {
    return scene.add(obj);
  }, function (obj) {
    scene.remove(obj);
    emptyObject(obj);
  }, _objectSpread2({
    objBindAttr: '__threeObj'
  }, options));
}

var colorStr2Hex = function colorStr2Hex(str) {
  return isNaN(str) ? parseInt(tinycolor(str).toHex(), 16) : str;
};

var colorAlpha = function colorAlpha(str) {
  return isNaN(str) ? tinycolor(str).getAlpha() : 1;
};

var autoColorScale = scaleOrdinal(schemePaired); // Autoset attribute colorField by colorByAccessor property
// If an object has already a color, don't set it
// Objects can be nodes or links

function autoColorObjects(objects, colorByAccessor, colorField) {
  if (!colorByAccessor || typeof colorField !== 'string') return;
  objects.filter(function (obj) {
    return !obj[colorField];
  }).forEach(function (obj) {
    obj[colorField] = autoColorScale(colorByAccessor(obj));
  });
}

function getDagDepths (_ref, idAccessor) {
  var nodes = _ref.nodes,
      links = _ref.links;

  var _ref2 = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {},
      _ref2$nodeFilter = _ref2.nodeFilter,
      nodeFilter = _ref2$nodeFilter === void 0 ? function () {
    return true;
  } : _ref2$nodeFilter,
      _ref2$onLoopError = _ref2.onLoopError,
      onLoopError = _ref2$onLoopError === void 0 ? function (loopIds) {
    throw "Invalid DAG structure! Found cycle in node path: ".concat(loopIds.join(' -> '), ".");
  } : _ref2$onLoopError;

  // linked graph
  var graph = {};
  nodes.forEach(function (node) {
    return graph[idAccessor(node)] = {
      data: node,
      out: [],
      depth: -1,
      skip: !nodeFilter(node)
    };
  });
  links.forEach(function (_ref3) {
    var source = _ref3.source,
        target = _ref3.target;
    var sourceId = getNodeId(source);
    var targetId = getNodeId(target);
    if (!graph.hasOwnProperty(sourceId)) throw "Missing source node with id: ".concat(sourceId);
    if (!graph.hasOwnProperty(targetId)) throw "Missing target node with id: ".concat(targetId);
    var sourceNode = graph[sourceId];
    var targetNode = graph[targetId];
    sourceNode.out.push(targetNode);

    function getNodeId(node) {
      return _typeof(node) === 'object' ? idAccessor(node) : node;
    }
  });
  var foundLoops = [];
  traverse(Object.values(graph));
  var nodeDepths = Object.assign.apply(Object, [{}].concat(_toConsumableArray(Object.entries(graph).filter(function (_ref4) {
    var _ref5 = _slicedToArray(_ref4, 2),
        node = _ref5[1];

    return !node.skip;
  }).map(function (_ref6) {
    var _ref7 = _slicedToArray(_ref6, 2),
        id = _ref7[0],
        node = _ref7[1];

    return _defineProperty({}, id, node.depth);
  }))));
  return nodeDepths;

  function traverse(nodes) {
    var nodeStack = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];
    var currentDepth = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    for (var i = 0, l = nodes.length; i < l; i++) {
      var node = nodes[i];

      if (nodeStack.indexOf(node) !== -1) {
        var _ret = function () {
          var loop = [].concat(_toConsumableArray(nodeStack.slice(nodeStack.indexOf(node))), [node]).map(function (d) {
            return idAccessor(d.data);
          });

          if (!foundLoops.some(function (foundLoop) {
            return foundLoop.length === loop.length && foundLoop.every(function (id, idx) {
              return id === loop[idx];
            });
          })) {
            foundLoops.push(loop);
            onLoopError(loop);
          }

          return "continue";
        }();

        if (_ret === "continue") continue;
      }

      if (currentDepth > node.depth) {
        // Don't unnecessarily revisit chunks of the graph
        node.depth = currentDepth;
        traverse(node.out, [].concat(_toConsumableArray(nodeStack), [node]), currentDepth + (node.skip ? 0 : 1));
      }
    }
  }
}

var three$1 = window.THREE ? window.THREE // Prefer consumption from global THREE, if exists
: {
  Group: Group,
  Mesh: Mesh,
  MeshLambertMaterial: MeshLambertMaterial,
  Color: Color,
  BufferGeometry: BufferGeometry,
  BufferAttribute: BufferAttribute,
  Matrix4: Matrix4,
  Vector3: Vector3,
  SphereBufferGeometry: SphereBufferGeometry,
  CylinderBufferGeometry: CylinderBufferGeometry,
  TubeBufferGeometry: TubeBufferGeometry,
  ConeBufferGeometry: ConeBufferGeometry,
  Line: Line,
  LineBasicMaterial: LineBasicMaterial,
  QuadraticBezierCurve3: QuadraticBezierCurve3,
  CubicBezierCurve3: CubicBezierCurve3,
  Box3: Box3
};
var ngraph = {
  graph: graph//,
  //forcelayout: forcelayout
};

var DAG_LEVEL_NODE_RATIO = 2; // support multiple method names for backwards threejs compatibility

var setAttributeFn = new three$1.BufferGeometry().setAttribute ? 'setAttribute' : 'addAttribute';
var applyMatrix4Fn = new three$1.BufferGeometry().applyMatrix4 ? 'applyMatrix4' : 'applyMatrix';
var ForceGraph = Kapsule({
  props: {
    jsonUrl: {
      onChange: function onChange(jsonUrl, state) {
        var _this = this;

        if (jsonUrl && !state.fetchingJson) {
          // Load data asynchronously
          state.fetchingJson = true;
          state.onLoading();
          fetch(jsonUrl).then(function (r) {
            return r.json();
          }).then(function (json) {
            state.fetchingJson = false;
            state.onFinishLoading(json);

            _this.graphData(json);
          });
        }
      },
      triggerUpdate: false
    },
    graphData: {
      "default": {
        nodes: [],
        links: []
      },
      onChange: function onChange(graphData, state) {
        state.engineRunning = false; // Pause simulation immediately
      }
    },
    numDimensions: {
      "default": 3,
      onChange: function onChange(numDim, state) {
        var chargeForce = state.d3ForceLayout.force('charge'); // Increase repulsion on 3D mode for improved spatial separation

        if (chargeForce) {
          chargeForce.strength(numDim > 2 ? -60 : -30);
        }

        if (numDim < 3) {
          eraseDimension(state.graphData.nodes, 'z');
        }

        if (numDim < 2) {
          eraseDimension(state.graphData.nodes, 'y');
        }

        function eraseDimension(nodes, dim) {
          nodes.forEach(function (d) {
            delete d[dim]; // position

            delete d["v".concat(dim)]; // velocity
          });
        }
      }
    },
    dagMode: {
      onChange: function onChange(dagMode, state) {
        // td, bu, lr, rl, zin, zout, radialin, radialout
        !dagMode && state.forceEngine === 'd3' && (state.graphData.nodes || []).forEach(function (n) {
          return n.fx = n.fy = n.fz = undefined;
        }); // unfix nodes when disabling dag mode
      }
    },
    dagLevelDistance: {},
    dagNodeFilter: {
      "default": function _default(node) {
        return true;
      }
    },
    onDagError: {
      triggerUpdate: false
    },
    nodeRelSize: {
      "default": 4
    },
    // volume per val unit
    nodeId: {
      "default": 'id'
    },
    nodeVal: {
      "default": 'val'
    },
    nodeResolution: {
      "default": 8
    },
    // how many slice segments in the sphere's circumference
    nodeColor: {
      "default": 'color'
    },
    nodeAutoColorBy: {},
    nodeOpacity: {
      "default": 0.75
    },
    nodeVisibility: {
      "default": true
    },
    nodeThreeObject: {},
    nodeThreeObjectExtend: {
      "default": false
    },
    linkSource: {
      "default": 'source'
    },
    linkTarget: {
      "default": 'target'
    },
    linkVisibility: {
      "default": true
    },
    linkColor: {
      "default": 'color'
    },
    linkAutoColorBy: {},
    linkOpacity: {
      "default": 0.2
    },
    linkWidth: {},
    // Rounded to nearest decimal. For falsy values use dimensionless line with 1px regardless of distance.
    linkResolution: {
      "default": 6
    },
    // how many radial segments in each line tube's geometry
    linkCurvature: {
      "default": 0,
      triggerUpdate: false
    },
    // line curvature radius (0: straight, 1: semi-circle)
    linkCurveRotation: {
      "default": 0,
      triggerUpdate: false
    },
    // line curve rotation along the line axis (0: interection with XY plane, PI: upside down)
    linkMaterial: {},
    linkThreeObject: {},
    linkThreeObjectExtend: {
      "default": false
    },
    linkPositionUpdate: {
      triggerUpdate: false
    },
    // custom function to call for updating the link's position. Signature: (threeObj, { start: { x, y, z},  end: { x, y, z }}, link). If the function returns a truthy value, the regular link position update will not run.
    linkDirectionalArrowLength: {
      "default": 0
    },
    linkDirectionalArrowColor: {},
    linkDirectionalArrowRelPos: {
      "default": 0.5,
      triggerUpdate: false
    },
    // value between 0<>1 indicating the relative pos along the (exposed) line
    linkDirectionalArrowResolution: {
      "default": 8
    },
    // how many slice segments in the arrow's conic circumference
    linkDirectionalParticles: {
      "default": 0
    },
    // animate photons travelling in the link direction
    linkDirectionalParticleSpeed: {
      "default": 0.01,
      triggerUpdate: false
    },
    // in link length ratio per frame
    linkDirectionalParticleWidth: {
      "default": 0.5
    },
    linkDirectionalParticleColor: {},
    linkDirectionalParticleResolution: {
      "default": 4
    },
    // how many slice segments in the particle sphere's circumference
    forceEngine: {
      "default": 'd3'
    },
    // d3 or ngraph
    d3AlphaMin: {
      "default": 0
    },
    d3AlphaDecay: {
      "default": 0.0228,
      triggerUpdate: false,
      onChange: function onChange(alphaDecay, state) {
        state.d3ForceLayout.alphaDecay(alphaDecay);
      }
    },
    d3AlphaTarget: {
      "default": 0,
      triggerUpdate: false,
      onChange: function onChange(alphaTarget, state) {
        state.d3ForceLayout.alphaTarget(alphaTarget);
      }
    },
    d3VelocityDecay: {
      "default": 0.4,
      triggerUpdate: false,
      onChange: function onChange(velocityDecay, state) {
        state.d3ForceLayout.velocityDecay(velocityDecay);
      }
    },
    ngraphPhysics: {
      "default": {
        // defaults from https://github.com/anvaka/ngraph.physics.simulator/blob/master/index.js
        timeStep: 20,
        gravity: -1.2,
        theta: 0.8,
        springLength: 30,
        springCoefficient: 0.0008,
        dragCoefficient: 0.02
      }
    },
    warmupTicks: {
      "default": 0,
      triggerUpdate: false
    },
    // how many times to tick the force engine at init before starting to render
    cooldownTicks: {
      "default": Infinity,
      triggerUpdate: false
    },
    cooldownTime: {
      "default": 15000,
      triggerUpdate: false
    },
    // ms
    onLoading: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onFinishLoading: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onUpdate: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onFinishUpdate: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onEngineTick: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onEngineStop: {
      "default": function _default() {},
      triggerUpdate: false
    }
  },
  methods: {
    refresh: function refresh(state) {
      state._flushObjects = true;

      state._rerender();

      return this;
    },
    // Expose d3 forces for external manipulation
    d3Force: function d3Force(state, forceName, forceFn) {
      if (forceFn === undefined) {
        return state.d3ForceLayout.force(forceName); // Force getter
      }

      state.d3ForceLayout.force(forceName, forceFn); // Force setter

      return this;
    },
    d3ReheatSimulation: function d3ReheatSimulation(state) {
      state.d3ForceLayout.alpha(1);
      this.resetCountdown();
      return this;
    },
    // reset cooldown state
    resetCountdown: function resetCountdown(state) {
      state.cntTicks = 0;
      state.startTickTime = new Date();
      state.engineRunning = true;
      return this;
    },
    tickFrame: function tickFrame(state) {
      var isD3Sim = state.forceEngine !== 'ngraph';

      if (state.engineRunning) {
        layoutTick();
      }

      updateArrows();
      updatePhotons();
      return this; //

      function layoutTick() {
        if (++state.cntTicks > state.cooldownTicks || new Date() - state.startTickTime > state.cooldownTime || isD3Sim && state.d3AlphaMin > 0 && state.d3ForceLayout.alpha() < state.d3AlphaMin) {
          state.engineRunning = false; // Stop ticking graph

          state.onEngineStop();
        } else {
          state.layout[isD3Sim ? 'tick' : 'step'](); // Tick it

          state.onEngineTick();
        } // Update nodes position


        state.graphData.nodes.forEach(function (node) {
          var obj = node.__threeObj;
          if (!obj) return;
          var pos = isD3Sim ? node : state.layout.getNodePosition(node[state.nodeId]);
          obj.position.x = pos.x;
          obj.position.y = pos.y || 0;
          obj.position.z = pos.z || 0;
        }); // Update links position

        var linkWidthAccessor = accessorFn(state.linkWidth);
        var linkCurvatureAccessor = accessorFn(state.linkCurvature);
        var linkCurveRotationAccessor = accessorFn(state.linkCurveRotation);
        var linkThreeObjectExtendAccessor = accessorFn(state.linkThreeObjectExtend);
        state.graphData.links.forEach(function (link) {
          var lineObj = link.__lineObj;
          if (!lineObj) return;
          var pos = isD3Sim ? link : state.layout.getLinkPosition(state.layout.graph.getLink(link.source, link.target).id);
          var start = pos[isD3Sim ? 'source' : 'from'];
          var end = pos[isD3Sim ? 'target' : 'to'];
          if (!start || !end || !start.hasOwnProperty('x') || !end.hasOwnProperty('x')) return; // skip invalid link

          calcLinkCurve(link); // calculate link curve for all links, including custom replaced, so it can be used in directional functionality

          var extendedObj = linkThreeObjectExtendAccessor(link);

          if (state.linkPositionUpdate && state.linkPositionUpdate(extendedObj ? lineObj.children[1] : lineObj, // pass child custom object if extending the default
          {
            start: {
              x: start.x,
              y: start.y,
              z: start.z
            },
            end: {
              x: end.x,
              y: end.y,
              z: end.z
            }
          }, link) && !extendedObj) {
            // exit if successfully custom updated position of non-extended obj
            return;
          }

          var curveResolution = 30; // # line segments

          var curve = link.__curve; // select default line obj if it's an extended group

          var line = lineObj.children.length ? lineObj.children[0] : lineObj;

          if (line.type === 'Line') {
            // Update line geometry
            if (!curve) {
              // straight line
              var linePos = line.geometry.getAttribute('position');

              if (!linePos || !linePos.array || linePos.array.length !== 6) {
                line.geometry[setAttributeFn]('position', linePos = new three$1.BufferAttribute(new Float32Array(2 * 3), 3));
              }

              linePos.array[0] = start.x;
              linePos.array[1] = start.y || 0;
              linePos.array[2] = start.z || 0;
              linePos.array[3] = end.x;
              linePos.array[4] = end.y || 0;
              linePos.array[5] = end.z || 0;
              linePos.needsUpdate = true;
            } else {
              // bezier curve line
              line.geometry.setFromPoints(curve.getPoints(curveResolution));
            }

            line.geometry.computeBoundingSphere();
          } else if (line.type === 'Mesh') {
            // Update cylinder geometry
            if (!curve) {
              // straight tube
              if (!line.geometry.type.match(/^Cylinder(Buffer)?Geometry$/)) {
                var linkWidth = Math.ceil(linkWidthAccessor(link) * 10) / 10;
                var r = linkWidth / 2;
                var geometry = new three$1.CylinderBufferGeometry(r, r, 1, state.linkResolution, 1, false);
                geometry[applyMatrix4Fn](new three$1.Matrix4().makeTranslation(0, 1 / 2, 0));
                geometry[applyMatrix4Fn](new three$1.Matrix4().makeRotationX(Math.PI / 2));
                line.geometry.dispose();
                line.geometry = geometry;
              }

              var vStart = new three$1.Vector3(start.x, start.y || 0, start.z || 0);
              var vEnd = new three$1.Vector3(end.x, end.y || 0, end.z || 0);
              var distance = vStart.distanceTo(vEnd);
              line.position.x = vStart.x;
              line.position.y = vStart.y;
              line.position.z = vStart.z;
              line.scale.z = distance;
              line.parent.localToWorld(vEnd); // lookAt requires world coords

              line.lookAt(vEnd);
            } else {
              // curved tube
              if (!line.geometry.type.match(/^Tube(Buffer)?Geometry$/)) {
                // reset object positioning
                line.position.set(0, 0, 0);
                line.rotation.set(0, 0, 0);
                line.scale.set(1, 1, 1);
              }

              var _linkWidth = Math.ceil(linkWidthAccessor(link) * 10) / 10;

              var _r = _linkWidth / 2;

              var _geometry = new three$1.TubeBufferGeometry(curve, curveResolution, _r, state.linkResolution, false);

              line.geometry.dispose();
              line.geometry = _geometry;
            }
          }
        }); //

        function calcLinkCurve(link) {
          var pos = isD3Sim ? link : state.layout.getLinkPosition(state.layout.graph.getLink(link.source, link.target).id);
          var start = pos[isD3Sim ? 'source' : 'from'];
          var end = pos[isD3Sim ? 'target' : 'to'];
          if (!start || !end || !start.hasOwnProperty('x') || !end.hasOwnProperty('x')) return; // skip invalid link

          var curvature = linkCurvatureAccessor(link);

          if (!curvature) {
            link.__curve = null; // Straight line
          } else {
            // bezier curve line (only for line types)
            var vStart = new three$1.Vector3(start.x, start.y || 0, start.z || 0);
            var vEnd = new three$1.Vector3(end.x, end.y || 0, end.z || 0);
            var l = vStart.distanceTo(vEnd); // line length

            var curve;
            var curveRotation = linkCurveRotationAccessor(link);

            if (l > 0) {
              var dx = end.x - start.x;
              var dy = end.y - start.y || 0;
              var vLine = new three$1.Vector3().subVectors(vEnd, vStart);
              var cp = vLine.clone().multiplyScalar(curvature).cross(dx !== 0 || dy !== 0 ? new three$1.Vector3(0, 0, 1) : new three$1.Vector3(0, 1, 0)) // avoid cross-product of parallel vectors (prefer Z, fallback to Y)
              .applyAxisAngle(vLine.normalize(), curveRotation) // rotate along line axis according to linkCurveRotation
              .add(new three$1.Vector3().addVectors(vStart, vEnd).divideScalar(2));
              curve = new three$1.QuadraticBezierCurve3(vStart, cp, vEnd);
            } else {
              // Same point, draw a loop
              var d = curvature * 70;
              var endAngle = -curveRotation; // Rotate clockwise (from Z angle perspective)

              var startAngle = endAngle + Math.PI / 2;
              curve = new three$1.CubicBezierCurve3(vStart, new three$1.Vector3(d * Math.cos(startAngle), d * Math.sin(startAngle), 0).add(vStart), new three$1.Vector3(d * Math.cos(endAngle), d * Math.sin(endAngle), 0).add(vStart), vEnd);
            }

            link.__curve = curve;
          }
        }
      }

      function updateArrows() {
        // update link arrow position
        var arrowRelPosAccessor = accessorFn(state.linkDirectionalArrowRelPos);
        var arrowLengthAccessor = accessorFn(state.linkDirectionalArrowLength);
        var nodeValAccessor = accessorFn(state.nodeVal);
        if(state.graphData.links === ""){
            state.graphData.links = [];
        }
        state.graphData.links.forEach(function (link) {
          var arrowObj = link.__arrowObj;
          if (!arrowObj) return;
          var pos = isD3Sim ? link : state.layout.getLinkPosition(state.layout.graph.getLink(link.source, link.target).id);
          var start = pos[isD3Sim ? 'source' : 'from'];
          var end = pos[isD3Sim ? 'target' : 'to'];
          if (!start || !end || !start.hasOwnProperty('x') || !end.hasOwnProperty('x')) return; // skip invalid link

          var startR = Math.sqrt(Math.max(0, nodeValAccessor(start) || 1)) * state.nodeRelSize;
          var endR = Math.sqrt(Math.max(0, nodeValAccessor(end) || 1)) * state.nodeRelSize;
          var arrowLength = arrowLengthAccessor(link);
          var arrowRelPos = arrowRelPosAccessor(link);
          var getPosAlongLine = link.__curve ? function (t) {
            return link.__curve.getPoint(t);
          } // interpolate along bezier curve
          : function (t) {
            // straight line: interpolate linearly
            var iplt = function iplt(dim, start, end, t) {
              return start[dim] + (end[dim] - start[dim]) * t || 0;
            };

            return {
              x: iplt('x', start, end, t),
              y: iplt('y', start, end, t),
              z: iplt('z', start, end, t)
            };
          };
          var lineLen = link.__curve ? link.__curve.getLength() : Math.sqrt(['x', 'y', 'z'].map(function (dim) {
            return Math.pow((end[dim] || 0) - (start[dim] || 0), 2);
          }).reduce(function (acc, v) {
            return acc + v;
          }, 0));
          var posAlongLine = startR + arrowLength + (lineLen - startR - endR - arrowLength) * arrowRelPos;
          var arrowHead = getPosAlongLine(posAlongLine / lineLen);
          var arrowTail = getPosAlongLine((posAlongLine - arrowLength) / lineLen);
          ['x', 'y', 'z'].forEach(function (dim) {
            return arrowObj.position[dim] = arrowTail[dim];
          });

          var headVec = _construct(three$1.Vector3, _toConsumableArray(['x', 'y', 'z'].map(function (c) {
            return arrowHead[c];
          })));

          arrowObj.parent.localToWorld(headVec); // lookAt requires world coords

          arrowObj.lookAt(headVec);
        });
      }

      function updatePhotons() {
        // update link particle positions
        var particleSpeedAccessor = accessorFn(state.linkDirectionalParticleSpeed);
        state.graphData.links.forEach(function (link) {
          var cyclePhotons = link.__photonsObj && link.__photonsObj.children;
          var singleHopPhotons = link.__singleHopPhotonsObj && link.__singleHopPhotonsObj.children;
          if ((!singleHopPhotons || !singleHopPhotons.length) && (!cyclePhotons || !cyclePhotons.length)) return;
          var pos = isD3Sim ? link : state.layout.getLinkPosition(state.layout.graph.getLink(link.source, link.target).id);
          var start = pos[isD3Sim ? 'source' : 'from'];
          var end = pos[isD3Sim ? 'target' : 'to'];
          if (!start || !end || !start.hasOwnProperty('x') || !end.hasOwnProperty('x')) return; // skip invalid link

          var particleSpeed = particleSpeedAccessor(link);
          var getPhotonPos = link.__curve ? function (t) {
            return link.__curve.getPoint(t);
          } // interpolate along bezier curve
          : function (t) {
            // straight line: interpolate linearly
            var iplt = function iplt(dim, start, end, t) {
              return start[dim] + (end[dim] - start[dim]) * t || 0;
            };

            return {
              x: iplt('x', start, end, t),
              y: iplt('y', start, end, t),
              z: iplt('z', start, end, t)
            };
          };
          var photons = [].concat(_toConsumableArray(cyclePhotons || []), _toConsumableArray(singleHopPhotons || []));
          photons.forEach(function (photon, idx) {
            var singleHop = photon.parent.__linkThreeObjType === 'singleHopPhotons';

            if (!photon.hasOwnProperty('__progressRatio')) {
              photon.__progressRatio = singleHop ? 0 : idx / cyclePhotons.length;
            }

            photon.__progressRatio += particleSpeed;

            if (photon.__progressRatio >= 1) {
              if (!singleHop) {
                photon.__progressRatio = photon.__progressRatio % 1;
              } else {
                // remove particle
                photon.parent.remove(photon);
                emptyObject(photon);
                return;
              }
            }

            var photonPosRatio = photon.__progressRatio;
            var pos = getPhotonPos(photonPosRatio);
            ['x', 'y', 'z'].forEach(function (dim) {
              return photon.position[dim] = pos[dim];
            });
          });
        });
      }
    },
    emitParticle: function emitParticle(state, link) {
      if (link) {
        if (!link.__singleHopPhotonsObj) {
          var obj = new three$1.Group();
          obj.__linkThreeObjType = 'singleHopPhotons';
          link.__singleHopPhotonsObj = obj;
          state.graphScene.add(obj);
        }

        var particleWidthAccessor = accessorFn(state.linkDirectionalParticleWidth);
        var photonR = Math.ceil(particleWidthAccessor(link) * 10) / 10 / 2;
        var numSegments = state.linkDirectionalParticleResolution;
        var particleGeometry = new three$1.SphereBufferGeometry(photonR, numSegments, numSegments);
        var linkColorAccessor = accessorFn(state.linkColor);
        var particleColorAccessor = accessorFn(state.linkDirectionalParticleColor);
        var photonColor = particleColorAccessor(link) || linkColorAccessor(link) || '#f0f0f0';
        var materialColor = new three$1.Color(colorStr2Hex(photonColor));
        var opacity = state.linkOpacity * 3;
        var particleMaterial = new three$1.MeshLambertMaterial({
          color: materialColor,
          transparent: true,
          opacity: opacity
        }); // add a single hop particle

        link.__singleHopPhotonsObj.add(new three$1.Mesh(particleGeometry, particleMaterial));
      }

      return this;
    },
    getGraphBbox: function getGraphBbox(state) {
      var nodeFilter = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : function () {
        return true;
      };
      if (!state.initialised) return null; // recursively collect all nested geometries bboxes

      var bboxes = function getBboxes(obj) {
        var bboxes = [];

        if (obj.geometry) {
          obj.geometry.computeBoundingBox();
          var box = new three$1.Box3();
          box.copy(obj.geometry.boundingBox).applyMatrix4(obj.matrixWorld);
          bboxes.push(box);
        }

        return bboxes.concat.apply(bboxes, _toConsumableArray((obj.children || []).filter(function (obj) {
          return !obj.hasOwnProperty('__graphObjType') || obj.__graphObjType === 'node' && nodeFilter(obj.__data);
        } // exclude filtered out nodes
        ).map(getBboxes)));
      }(state.graphScene);

      if (!bboxes.length) return null; // extract global x,y,z min/max

      return Object.assign.apply(Object, _toConsumableArray(['x', 'y', 'z'].map(function (c) {
        return _defineProperty({}, c, [min(bboxes, function (bb) {
          return bb.min[c];
        }), max(bboxes, function (bb) {
          return bb.max[c];
        })]);
      })));
    }
  },
  stateInit: function stateInit() {
    return {
      d3ForceLayout: forceSimulation().force('link', forceLink()).force('charge', forceManyBody()).force('center', forceCenter()).force('dagRadial', null).stop(),
      engineRunning: false
    };
  },
  init: function init(threeObj, state) {
    // Main three object to manipulate
    state.graphScene = threeObj;
  },
  update: function update(state, changedProps) {
    var hasAnyPropChanged = function hasAnyPropChanged(propList) {
      return propList.some(function (p) {
        return changedProps.hasOwnProperty(p);
      });
    };

    state.engineRunning = false; // pause simulation

    state.onUpdate();

    if (state.nodeAutoColorBy !== null && hasAnyPropChanged(['nodeAutoColorBy', 'graphData', 'nodeColor'])) {
      // Auto add color to uncolored nodes
      autoColorObjects(state.graphData.nodes, accessorFn(state.nodeAutoColorBy), state.nodeColor);
    }

    if (state.linkAutoColorBy !== null && hasAnyPropChanged(['linkAutoColorBy', 'graphData', 'linkColor'])) {
      // Auto add color to uncolored links
      autoColorObjects(state.graphData.links, accessorFn(state.linkAutoColorBy), state.linkColor);
    } // Digest nodes WebGL objects


    if (state._flushObjects || hasAnyPropChanged(['graphData', 'nodeThreeObject', 'nodeThreeObjectExtend', 'nodeVal', 'nodeColor', 'nodeVisibility', 'nodeRelSize', 'nodeResolution', 'nodeOpacity'])) {
      var customObjectAccessor = accessorFn(state.nodeThreeObject);
      var customObjectExtendAccessor = accessorFn(state.nodeThreeObjectExtend);
      var valAccessor = accessorFn(state.nodeVal);
      var colorAccessor = accessorFn(state.nodeColor);
      var visibilityAccessor = accessorFn(state.nodeVisibility);
      var sphereGeometries = {}; // indexed by node value

      var sphereMaterials = {}; // indexed by color

      threeDigest(state.graphData.nodes.filter(visibilityAccessor), state.graphScene, {
        purge: state._flushObjects || hasAnyPropChanged([// recreate objects if any of these props have changed
        'nodeThreeObject', 'nodeThreeObjectExtend']),
        objFilter: function objFilter(obj) {
          return obj.__graphObjType === 'node';
        },
        createObj: function createObj(node) {
          var customObj = customObjectAccessor(node);
          var extendObj = customObjectExtendAccessor(node);

          if (customObj && state.nodeThreeObject === customObj) {
            // clone object if it's a shared object among all nodes
            customObj = customObj.clone();
          }

          var obj;

          if (customObj && !extendObj) {
            obj = customObj;
          } else {
            // Add default object (sphere mesh)
            obj = new three$1.Mesh();
            obj.__graphDefaultObj = true;

            if (customObj && extendObj) {
              obj.add(customObj); // extend default with custom
            }
          }

          obj.__graphObjType = 'node'; // Add object type

          return obj;
        },
        updateObj: function updateObj(obj, node) {
          if (obj.__graphDefaultObj) {
            // bypass internal updates for custom node objects
            var val = valAccessor(node) || 1;
            var radius = Math.cbrt(val) * state.nodeRelSize;
            var numSegments = state.nodeResolution;

            if (!obj.geometry.type.match(/^Sphere(Buffer)?Geometry$/) || obj.geometry.parameters.radius !== radius || obj.geometry.parameters.widthSegments !== numSegments) {
              if (!sphereGeometries.hasOwnProperty(val)) {
                sphereGeometries[val] = new three$1.SphereBufferGeometry(radius, numSegments, numSegments);
              }

              obj.geometry.dispose();
              obj.geometry = sphereGeometries[val];
            }

            var color = colorAccessor(node);
            var materialColor = new three$1.Color(colorStr2Hex(color || '#ffffaa'));
            var opacity = state.nodeOpacity * colorAlpha(color);

            if (obj.material.type !== 'MeshLambertMaterial' || !obj.material.color.equals(materialColor) || obj.material.opacity !== opacity) {
              if (!sphereMaterials.hasOwnProperty(color)) {
                sphereMaterials[color] = new three$1.MeshLambertMaterial({
                  color: materialColor,
                  transparent: true,
                  opacity: opacity
                });
              }

              obj.material.dispose();
              obj.material = sphereMaterials[color];
            }
          }
        }
      });
    } // Digest links WebGL objects


    if (state._flushObjects || hasAnyPropChanged(['graphData', 'linkThreeObject', 'linkThreeObjectExtend', 'linkMaterial', 'linkColor', 'linkWidth', 'linkVisibility', 'linkResolution', 'linkOpacity', 'linkDirectionalArrowLength', 'linkDirectionalArrowColor', 'linkDirectionalArrowResolution', 'linkDirectionalParticles', 'linkDirectionalParticleWidth', 'linkDirectionalParticleColor', 'linkDirectionalParticleResolution'])) {
      var _customObjectAccessor = accessorFn(state.linkThreeObject);

      var _customObjectExtendAccessor = accessorFn(state.linkThreeObjectExtend);

      var customMaterialAccessor = accessorFn(state.linkMaterial);

      var _visibilityAccessor = accessorFn(state.linkVisibility);

      var _colorAccessor = accessorFn(state.linkColor);

      var widthAccessor = accessorFn(state.linkWidth);
      var cylinderGeometries = {}; // indexed by link width

      var lambertLineMaterials = {}; // for cylinder objects, indexed by link color

      var basicLineMaterials = {}; // for line objects, indexed by link color

      var visibleLinks = state.graphData.links.filter(_visibilityAccessor); // lines digest cycle

      threeDigest(visibleLinks, state.graphScene, {
        objBindAttr: '__lineObj',
        purge: state._flushObjects || hasAnyPropChanged([// recreate objects if any of these props have changed
        'linkThreeObject', 'linkThreeObjectExtend', 'linkWidth']),
        objFilter: function objFilter(obj) {
          return obj.__graphObjType === 'link';
        },
        createObj: function createObj(link) {
          var customObj = _customObjectAccessor(link);

          var extendObj = _customObjectExtendAccessor(link);

          if (customObj && state.linkThreeObject === customObj) {
            // clone object if it's a shared object among all links
            customObj = customObj.clone();
          }

          var defaultObj;

          if (!customObj || extendObj) {
            // construct default line obj
            var useCylinder = !!widthAccessor(link);

            if (useCylinder) {
              defaultObj = new three$1.Mesh();
            } else {
              // Use plain line (constant width)
              var lineGeometry = new three$1.BufferGeometry();
              lineGeometry[setAttributeFn]('position', new three$1.BufferAttribute(new Float32Array(2 * 3), 3));
              defaultObj = new three$1.Line(lineGeometry);
            }
          }

          var obj;

          if (!customObj) {
            obj = defaultObj;
            obj.__graphDefaultObj = true;
          } else {
            if (!extendObj) {
              // use custom object
              obj = customObj;
            } else {
              // extend default with custom in a group
              obj = new three$1.Group();
              obj.__graphDefaultObj = true;
              obj.add(defaultObj);
              obj.add(customObj);
            }
          }

          obj.renderOrder = 10; // Prevent visual glitches of dark lines on top of nodes by rendering them last

          obj.__graphObjType = 'link'; // Add object type

          return obj;
        },
        updateObj: function updateObj(updObj, link) {
          if (updObj.__graphDefaultObj) {
            // bypass internal updates for custom link objects
            // select default object if it's an extended group
            var obj = updObj.children.length ? updObj.children[0] : updObj;
            var linkWidth = Math.ceil(widthAccessor(link) * 10) / 10;
            var useCylinder = !!linkWidth;

            if (useCylinder) {
              var r = linkWidth / 2;
              var numSegments = state.linkResolution;

              if (!obj.geometry.type.match(/^Cylinder(Buffer)?Geometry$/) || obj.geometry.parameters.radiusTop !== r || obj.geometry.parameters.radialSegments !== numSegments) {
                if (!cylinderGeometries.hasOwnProperty(linkWidth)) {
                  var geometry = new three$1.CylinderBufferGeometry(r, r, 1, numSegments, 1, false);
                  geometry[applyMatrix4Fn](new three$1.Matrix4().makeTranslation(0, 1 / 2, 0));
                  geometry[applyMatrix4Fn](new three$1.Matrix4().makeRotationX(Math.PI / 2));
                  cylinderGeometries[linkWidth] = geometry;
                }

                obj.geometry.dispose();
                obj.geometry = cylinderGeometries[linkWidth];
              }
            }

            var customMaterial = customMaterialAccessor(link);

            if (customMaterial) {
              obj.material = customMaterial;
            } else {
              var color = _colorAccessor(link);

              var materialColor = new three$1.Color(colorStr2Hex(color || '#f0f0f0'));
              var opacity = state.linkOpacity * colorAlpha(color);
              var materialType = useCylinder ? 'MeshLambertMaterial' : 'LineBasicMaterial';

              if (obj.material.type !== materialType || !obj.material.color.equals(materialColor) || obj.material.opacity !== opacity) {
                var lineMaterials = useCylinder ? lambertLineMaterials : basicLineMaterials;

                if (!lineMaterials.hasOwnProperty(color)) {
                  lineMaterials[color] = new three$1[materialType]({
                    color: materialColor,
                    transparent: opacity < 1,
                    opacity: opacity,
                    depthWrite: opacity >= 1 // Prevent transparency issues

                  });
                }

                obj.material.dispose();
                obj.material = lineMaterials[color];
              }
            }
          }
        }
      }); // Arrows digest cycle

      if (state.linkDirectionalArrowLength || changedProps.hasOwnProperty('linkDirectionalArrowLength')) {
        var arrowLengthAccessor = accessorFn(state.linkDirectionalArrowLength);
        var arrowColorAccessor = accessorFn(state.linkDirectionalArrowColor);
        threeDigest(visibleLinks.filter(arrowLengthAccessor), state.graphScene, {
          objBindAttr: '__arrowObj',
          objFilter: function objFilter(obj) {
            return obj.__linkThreeObjType === 'arrow';
          },
          createObj: function createObj() {
            var obj = new three$1.Mesh(undefined, new three$1.MeshLambertMaterial({
              transparent: true
            }));
            obj.__linkThreeObjType = 'arrow'; // Add object type

            return obj;
          },
          updateObj: function updateObj(obj, link) {
            var arrowLength = arrowLengthAccessor(link);
            var numSegments = state.linkDirectionalArrowResolution;

            if (!obj.geometry.type.match(/^Cone(Buffer)?Geometry$/) || obj.geometry.parameters.height !== arrowLength || obj.geometry.parameters.radialSegments !== numSegments) {
              var coneGeometry = new three$1.ConeBufferGeometry(arrowLength * 0.25, arrowLength, numSegments); // Correct orientation

              coneGeometry.translate(0, arrowLength / 2, 0);
              coneGeometry.rotateX(Math.PI / 2);
              obj.geometry.dispose();
              obj.geometry = coneGeometry;
            }

            obj.material.color = new three$1.Color(arrowColorAccessor(link) || _colorAccessor(link) || '#f0f0f0');
            obj.material.opacity = state.linkOpacity * 3;
          }
        });
      } // Photon particles digest cycle


      if (state.linkDirectionalParticles || changedProps.hasOwnProperty('linkDirectionalParticles')) {
        var particlesAccessor = accessorFn(state.linkDirectionalParticles);
        var particleWidthAccessor = accessorFn(state.linkDirectionalParticleWidth);
        var particleColorAccessor = accessorFn(state.linkDirectionalParticleColor);
        var particleMaterials = {}; // indexed by link color

        var particleGeometries = {}; // indexed by particle width

        threeDigest(visibleLinks.filter(particlesAccessor), state.graphScene, {
          objBindAttr: '__photonsObj',
          objFilter: function objFilter(obj) {
            return obj.__linkThreeObjType === 'photons';
          },
          createObj: function createObj() {
            var obj = new three$1.Group();
            obj.__linkThreeObjType = 'photons'; // Add object type

            return obj;
          },
          updateObj: function updateObj(obj, link) {
            var numPhotons = Math.round(Math.abs(particlesAccessor(link)));
            var curPhoton = !!obj.children.length && obj.children[0];
            var photonR = Math.ceil(particleWidthAccessor(link) * 10) / 10 / 2;
            var numSegments = state.linkDirectionalParticleResolution;
            var particleGeometry;

            if (curPhoton && curPhoton.geometry.parameters.radius === photonR && curPhoton.geometry.parameters.widthSegments === numSegments) {
              particleGeometry = curPhoton.geometry;
            } else {
              if (!particleGeometries.hasOwnProperty(photonR)) {
                particleGeometries[photonR] = new three$1.SphereBufferGeometry(photonR, numSegments, numSegments);
              }

              particleGeometry = particleGeometries[photonR];
              curPhoton && curPhoton.geometry.dispose();
            }

            var photonColor = particleColorAccessor(link) || _colorAccessor(link) || '#f0f0f0';
            var materialColor = new three$1.Color(colorStr2Hex(photonColor));
            var opacity = state.linkOpacity * 3;
            var particleMaterial;

            if (curPhoton && curPhoton.material.color.equals(materialColor) && curPhoton.material.opacity === opacity) {
              particleMaterial = curPhoton.material;
            } else {
              if (!particleMaterials.hasOwnProperty(photonColor)) {
                particleMaterials[photonColor] = new three$1.MeshLambertMaterial({
                  color: materialColor,
                  transparent: true,
                  opacity: opacity
                });
              }

              particleMaterial = particleMaterials[photonColor];
              curPhoton && curPhoton.material.dispose();
            } // digest cycle for each photon


            threeDigest(_toConsumableArray(new Array(numPhotons)).map(function (_, idx) {
              return {
                idx: idx
              };
            }), obj, {
              idAccessor: function idAccessor(d) {
                return d.idx;
              },
              createObj: function createObj() {
                return new three$1.Mesh(particleGeometry, particleMaterial);
              },
              updateObj: function updateObj(obj) {
                obj.geometry = particleGeometry;
                obj.material = particleMaterial;
              }
            });
          }
        });
      }
    }

    state._flushObjects = false; // reset objects refresh flag
    // simulation engine

    if (hasAnyPropChanged(['graphData', 'nodeId', 'linkSource', 'linkTarget', 'numDimensions', 'forceEngine', 'dagMode', 'dagNodeFilter', 'dagLevelDistance'])) {
      state.engineRunning = false; // Pause simulation
      // parse links

      state.graphData.links.forEach(function (link) {
        link.source = link[state.linkSource];
        link.target = link[state.linkTarget];
      }); // Feed data to force-directed layout

      var isD3Sim = state.forceEngine !== 'ngraph';
      var layout;

      if (isD3Sim) {
        // D3-force
        (layout = state.d3ForceLayout).stop().alpha(1) // re-heat the simulation
        .numDimensions(state.numDimensions).nodes(state.graphData.nodes); // add links (if link force is still active)

        var linkForce = state.d3ForceLayout.force('link');

        if (linkForce) {
          linkForce.id(function (d) {
            return d[state.nodeId];
          }).links(state.graphData.links);
        } // setup dag force constraints


        var nodeDepths = state.dagMode && getDagDepths(state.graphData, function (node) {
          return node[state.nodeId];
        }, {
          nodeFilter: state.dagNodeFilter,
          onLoopError: state.onDagError || undefined
        });
        var maxDepth = Math.max.apply(Math, _toConsumableArray(Object.values(nodeDepths || [])));
        var dagLevelDistance = state.dagLevelDistance || state.graphData.nodes.length / (maxDepth || 1) * DAG_LEVEL_NODE_RATIO * (['radialin', 'radialout'].indexOf(state.dagMode) !== -1 ? 0.7 : 1); // Fix nodes to x,y,z for dag mode

        if (state.dagMode) {
          var getFFn = function getFFn(fix, invert) {
            return function (node) {
              return !fix ? undefined : (nodeDepths[node[state.nodeId]] - maxDepth / 2) * dagLevelDistance * (invert ? -1 : 1);
            };
          };

          var fxFn = getFFn(['lr', 'rl'].indexOf(state.dagMode) !== -1, state.dagMode === 'rl');
          var fyFn = getFFn(['td', 'bu'].indexOf(state.dagMode) !== -1, state.dagMode === 'td');
          var fzFn = getFFn(['zin', 'zout'].indexOf(state.dagMode) !== -1, state.dagMode === 'zout');
          state.graphData.nodes.filter(state.dagNodeFilter).forEach(function (node) {
            node.fx = fxFn(node);
            node.fy = fyFn(node);
            node.fz = fzFn(node);
          });
        } // Use radial force for radial dags


        state.d3ForceLayout.force('dagRadial', ['radialin', 'radialout'].indexOf(state.dagMode) !== -1 ? forceRadial(function (node) {
          var nodeDepth = nodeDepths[node[state.nodeId]] || -1;
          return (state.dagMode === 'radialin' ? maxDepth - nodeDepth : nodeDepth) * dagLevelDistance;
        }).strength(function (node) {
          return state.dagNodeFilter(node) ? 1 : 0;
        }) : null);
      } else {
        // ngraph
        var _graph = ngraph.graph();

        state.graphData.nodes.forEach(function (node) {
          _graph.addNode(node[state.nodeId]);
        });
        state.graphData.links.forEach(function (link) {
          _graph.addLink(link.source, link.target);
        });
        /*
        layout = ngraph.forcelayout(_graph, _objectSpread2({
          dimensions: state.numDimensions
        }, state.ngraphPhysics));
          */
        layout.graph = _graph; // Attach graph reference to layout
      }

      for (var i = 0; i < state.warmupTicks && !(isD3Sim && state.d3AlphaMin > 0 && state.d3ForceLayout.alpha() < state.d3AlphaMin); i++) {
        layout[isD3Sim ? "tick" : "step"]();
      } // Initial ticks before starting to render


      state.layout = layout;
      this.resetCountdown();
    }

    state.engineRunning = true; // resume simulation

    state.onFinishUpdate();
  }
});

function fromKapsule (kapsule) {
  var baseClass = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : Object;
  var initKapsuleWithSelf = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  var FromKapsule = /*#__PURE__*/function (_baseClass) {
    _inherits(FromKapsule, _baseClass);

    var _super = _createSuper(FromKapsule);

    function FromKapsule() {
      var _this;

      _classCallCheck(this, FromKapsule);

      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

      _this = _super.call.apply(_super, [this].concat(args));
      _this.__kapsuleInstance = kapsule().apply(void 0, [].concat(_toConsumableArray(initKapsuleWithSelf ? [_assertThisInitialized(_this)] : []), args));
      return _this;
    }

    return _createClass(FromKapsule);
  }(baseClass); // attach kapsule props/methods to class prototype


  Object.keys(kapsule()).forEach(function (m) {
    return FromKapsule.prototype[m] = function () {
      var _this$__kapsuleInstan;

      var returnVal = (_this$__kapsuleInstan = this.__kapsuleInstance)[m].apply(_this$__kapsuleInstan, arguments);

      return returnVal === this.__kapsuleInstance ? this // chain based on this class, not the kapsule obj
      : returnVal;
    };
  });
  return FromKapsule;
}

var three = window.THREE ? window.THREE : {
  Group: Group
}; // Prefer consumption from global THREE, if exists
var threeForcegraph = fromKapsule(ForceGraph, three.Group, true);

export { threeForcegraph as default };
