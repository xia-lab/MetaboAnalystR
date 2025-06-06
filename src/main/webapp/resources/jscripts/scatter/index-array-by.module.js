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
  var _i = arr && (typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"]);

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

function _toPrimitive(input, hint) {
  if (typeof input !== "object" || input === null) return input;
  var prim = input[Symbol.toPrimitive];

  if (prim !== undefined) {
    var res = prim.call(input, hint || "default");
    if (typeof res !== "object") return res;
    throw new TypeError("@@toPrimitive must return a primitive value.");
  }

  return (hint === "string" ? String : Number)(input);
}

function _toPropertyKey(arg) {
  var key = _toPrimitive(arg, "string");

  return typeof key === "symbol" ? key : String(key);
}

var index = (function () {
  var list = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : [];
  var keyAccessors = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];
  var multiItem = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : true;
  var flattenKeys = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : false;
  var keys = (keyAccessors instanceof Array ? keyAccessors.length ? keyAccessors : [undefined] : [keyAccessors]).map(function (key) {
    return {
      keyAccessor: key,
      isProp: !(key instanceof Function)
    };
  });
  var indexedResult = list.reduce(function (res, item) {
    var iterObj = res;
    var itemVal = item;
    keys.forEach(function (_ref, idx) {
      var keyAccessor = _ref.keyAccessor,
          isProp = _ref.isProp;
      var key;

      if (isProp) {
        var _itemVal = itemVal,
            propVal = _itemVal[keyAccessor],
            rest = _objectWithoutProperties(_itemVal, [keyAccessor].map(_toPropertyKey));

        key = propVal;
        itemVal = rest;
      } else {
        key = keyAccessor(itemVal, idx);
      }

      if (idx + 1 < keys.length) {
        if (!iterObj.hasOwnProperty(key)) {
          iterObj[key] = {};
        }

        iterObj = iterObj[key];
      } else {
        // Leaf key
        if (multiItem) {
          if (!iterObj.hasOwnProperty(key)) {
            iterObj[key] = [];
          }

          iterObj[key].push(itemVal);
        } else {
          iterObj[key] = itemVal;
        }
      }
    });
    return res;
  }, {});

  if (multiItem instanceof Function) {
    // Reduce leaf multiple values
    (function reduce(node) {
      var level = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 1;

      if (level === keys.length) {
        Object.keys(node).forEach(function (k) {
          return node[k] = multiItem(node[k]);
        });
      } else {
        Object.values(node).forEach(function (child) {
          return reduce(child, level + 1);
        });
      }
    })(indexedResult); // IIFE

  }

  var result = indexedResult;

  if (flattenKeys) {
    // flatten into array
    result = [];

    (function flatten(node) {
      var accKeys = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];

      if (accKeys.length === keys.length) {
        result.push({
          keys: accKeys,
          vals: node
        });
      } else {
        Object.entries(node).forEach(function (_ref2) {
          var _ref3 = _slicedToArray(_ref2, 2),
              key = _ref3[0],
              val = _ref3[1];

          return flatten(val, [].concat(_toConsumableArray(accKeys), [key]));
        });
      }
    })(indexedResult); //IIFE


    if (keyAccessors instanceof Array && keyAccessors.length === 0 && result.length === 1) {
      // clear keys if there's no key accessors (single result)
      result[0].keys = [];
    }
  }

  return result;
});

export default index;
