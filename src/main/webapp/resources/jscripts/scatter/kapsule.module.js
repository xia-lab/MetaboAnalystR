import debounce from './debounce.js';

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

function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
}

function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
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

function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

var Prop = /*#__PURE__*/_createClass(function Prop(name, _ref) {
  var _ref$default = _ref["default"],
      defaultVal = _ref$default === void 0 ? null : _ref$default,
      _ref$triggerUpdate = _ref.triggerUpdate,
      triggerUpdate = _ref$triggerUpdate === void 0 ? true : _ref$triggerUpdate,
      _ref$onChange = _ref.onChange,
      onChange = _ref$onChange === void 0 ? function (newVal, state) {} : _ref$onChange;

  _classCallCheck(this, Prop);

  this.name = name;
  this.defaultVal = defaultVal;
  this.triggerUpdate = triggerUpdate;
  this.onChange = onChange;
});

function index (_ref2) {
  var _ref2$stateInit = _ref2.stateInit,
      stateInit = _ref2$stateInit === void 0 ? function () {
    return {};
  } : _ref2$stateInit,
      _ref2$props = _ref2.props,
      rawProps = _ref2$props === void 0 ? {} : _ref2$props,
      _ref2$methods = _ref2.methods,
      methods = _ref2$methods === void 0 ? {} : _ref2$methods,
      _ref2$aliases = _ref2.aliases,
      aliases = _ref2$aliases === void 0 ? {} : _ref2$aliases,
      _ref2$init = _ref2.init,
      initFn = _ref2$init === void 0 ? function () {} : _ref2$init,
      _ref2$update = _ref2.update,
      updateFn = _ref2$update === void 0 ? function () {} : _ref2$update;
  // Parse props into Prop instances
  var props = Object.keys(rawProps).map(function (propName) {
    return new Prop(propName, rawProps[propName]);
  });
  return function () {
    var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
    // Holds component state
    var state = Object.assign({}, stateInit instanceof Function ? stateInit(options) : stateInit, // Support plain objects for backwards compatibility
    {
      initialised: false
    }); // keeps track of which props triggered an update

    var changedProps = {}; // Component constructor

    function comp(nodeElement) {
      initStatic(nodeElement, options);
      digest();
      return comp;
    }

    var initStatic = function initStatic(nodeElement, options) {
      initFn.call(comp, nodeElement, state, options);
      state.initialised = true;
    };

    var digest = debounce(function () {
      if (!state.initialised) {
        return;
      }

      updateFn.call(comp, state, changedProps);
      changedProps = {};
    }, 1); // Getter/setter methods

    props.forEach(function (prop) {
      comp[prop.name] = getSetProp(prop);

      function getSetProp(_ref3) {
        var prop = _ref3.name,
            _ref3$triggerUpdate = _ref3.triggerUpdate,
            redigest = _ref3$triggerUpdate === void 0 ? false : _ref3$triggerUpdate,
            _ref3$onChange = _ref3.onChange,
            onChange = _ref3$onChange === void 0 ? function (newVal, state) {} : _ref3$onChange,
            _ref3$defaultVal = _ref3.defaultVal,
            defaultVal = _ref3$defaultVal === void 0 ? null : _ref3$defaultVal;
        return function (_) {
          var curVal = state[prop];

          if (!arguments.length) {
            return curVal;
          } // Getter mode


          var val = _ === undefined ? defaultVal : _; // pick default if value passed is undefined

          state[prop] = val;
          onChange.call(comp, val, state, curVal); // track changed props

          !changedProps.hasOwnProperty(prop) && (changedProps[prop] = curVal);

          if (redigest) {
            digest();
          }

          return comp;
        };
      }
    }); // Other methods

    Object.keys(methods).forEach(function (methodName) {
      comp[methodName] = function () {
        var _methods$methodName;

        for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
          args[_key] = arguments[_key];
        }

        return (_methods$methodName = methods[methodName]).call.apply(_methods$methodName, [comp, state].concat(args));
      };
    }); // Link aliases

    Object.entries(aliases).forEach(function (_ref4) {
      var _ref5 = _slicedToArray(_ref4, 2),
          alias = _ref5[0],
          target = _ref5[1];

      return comp[alias] = comp[target];
    }); // Reset all component props to their default value

    comp.resetProps = function () {
      props.forEach(function (prop) {
        comp[prop.name](prop.defaultVal);
      });
      return comp;
    }; //


    comp.resetProps(); // Apply all prop defaults

    state._rerender = digest; // Expose digest method

    return comp;
  };
}

export { index as default };
