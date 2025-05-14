import indexBy from './index-array-by.module.js';

function ownKeys(object, enumerableOnly) {
  var keys = Object.keys(object);

  if (Object.getOwnPropertySymbols) {
    var symbols = Object.getOwnPropertySymbols(object);

    if (enumerableOnly) {
      symbols = symbols.filter(function (sym) {
        return Object.getOwnPropertyDescriptor(object, sym).enumerable;
      });
    }

    keys.push.apply(keys, symbols);
  }

  return keys;
}

function _objectSpread2(target) {
  for (var i = 1; i < arguments.length; i++) {
    var source = arguments[i] != null ? arguments[i] : {};

    if (i % 2) {
      ownKeys(Object(source), true).forEach(function (key) {
        _defineProperty(target, key, source[key]);
      });
    } else if (Object.getOwnPropertyDescriptors) {
      Object.defineProperties(target, Object.getOwnPropertyDescriptors(source));
    } else {
      ownKeys(Object(source)).forEach(function (key) {
        Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key));
      });
    }
  }

  return target;
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

function diffArrays(prev, next, idAccessor) {
  var result = {
    enter: [],
    update: [],
    exit: []
  };

  if (!idAccessor) {
    // use object references for comparison
    var prevSet = new Set(prev);
    var nextSet = new Set(next);
    new Set([].concat(_toConsumableArray(prevSet), _toConsumableArray(nextSet))).forEach(function (item) {
      var type = !prevSet.has(item) ? 'enter' : !nextSet.has(item) ? 'exit' : 'update';
      result[type].push(type === 'update' ? [item, item] : item);
    });
  } else {
    // compare by id (duplicate keys are ignored)
    var prevById = indexBy(prev, idAccessor, false);
    var nextById = indexBy(next, idAccessor, false);
    var byId = Object.assign({}, prevById, nextById);
    Object.entries(byId).forEach(function (_ref) {
      var _ref2 = _slicedToArray(_ref, 2),
          id = _ref2[0],
          item = _ref2[1];

      var type = !prevById.hasOwnProperty(id) ? 'enter' : !nextById.hasOwnProperty(id) ? 'exit' : 'update';
      result[type].push(type === 'update' ? [prevById[id], nextById[id]] : item);
    });
  }

  return result;
}

function dataBindDiff(data, existingObjs, _ref3) {
  var _ref3$objBindAttr = _ref3.objBindAttr,
      objBindAttr = _ref3$objBindAttr === void 0 ? '__obj' : _ref3$objBindAttr,
      _ref3$dataBindAttr = _ref3.dataBindAttr,
      dataBindAttr = _ref3$dataBindAttr === void 0 ? '__data' : _ref3$dataBindAttr,
      idAccessor = _ref3.idAccessor,
      _ref3$purge = _ref3.purge,
      purge = _ref3$purge === void 0 ? false : _ref3$purge;

  var isObjValid = function isObjValid(obj) {
    return obj.hasOwnProperty(dataBindAttr);
  };

  var removeObjs = existingObjs.filter(function (obj) {
    return !isObjValid(obj);
  });
  var prevD = existingObjs.filter(isObjValid).map(function (obj) {
    return obj[dataBindAttr];
  });
  var nextD = data;
  var diff = purge ? {
    enter: nextD,
    exit: prevD,
    update: []
  } // don't diff data in purge mode
  : diffArrays(prevD, nextD, idAccessor);
  diff.update = diff.update.map(function (_ref4) {
    var _ref5 = _slicedToArray(_ref4, 2),
        prevD = _ref5[0],
        nextD = _ref5[1];

    if (prevD !== nextD) {
      // transfer obj to new data point (if different)
      nextD[objBindAttr] = prevD[objBindAttr];
      nextD[objBindAttr][dataBindAttr] = nextD;
    }

    return nextD;
  });
  diff.exit = diff.exit.concat(removeObjs.map(function (obj) {
    return _defineProperty({}, objBindAttr, obj);
  }));
  return diff;
}

function viewDigest(data, existingObjs, // list
appendObj, // item => {...} function
removeObj, // item => {...} function
_ref7) {
  var _ref7$createObj = _ref7.createObj,
      createObj = _ref7$createObj === void 0 ? function (d) {
    return {};
  } : _ref7$createObj,
      _ref7$updateObj = _ref7.updateObj,
      updateObj = _ref7$updateObj === void 0 ? function (obj, d) {} : _ref7$updateObj,
      _ref7$exitObj = _ref7.exitObj,
      exitObj = _ref7$exitObj === void 0 ? function (obj) {} : _ref7$exitObj,
      _ref7$objBindAttr = _ref7.objBindAttr,
      objBindAttr = _ref7$objBindAttr === void 0 ? '__obj' : _ref7$objBindAttr,
      _ref7$dataBindAttr = _ref7.dataBindAttr,
      dataBindAttr = _ref7$dataBindAttr === void 0 ? '__data' : _ref7$dataBindAttr,
      dataDiffOptions = _objectWithoutProperties(_ref7, ["createObj", "updateObj", "exitObj", "objBindAttr", "dataBindAttr"]);

  var _dataBindDiff = dataBindDiff(data, existingObjs, _objectSpread2({
    objBindAttr: objBindAttr,
    dataBindAttr: dataBindAttr
  }, dataDiffOptions)),
      enter = _dataBindDiff.enter,
      update = _dataBindDiff.update,
      exit = _dataBindDiff.exit; // Remove exiting points


  exit.forEach(function (d) {
    var obj = d[objBindAttr];
    delete d[objBindAttr]; // unbind obj

    exitObj(obj);
    removeObj(obj);
  });
  var newObjs = createObjs(enter);
  var pointsData = [].concat(_toConsumableArray(enter), _toConsumableArray(update));
  updateObjs(pointsData); // Add new points

  newObjs.forEach(appendObj); //

  function createObjs(data) {
    var newObjs = [];
    data.forEach(function (d) {
      var obj = createObj(d);

      if (obj) {
        obj[dataBindAttr] = d;
        d[objBindAttr] = obj;
        newObjs.push(obj);
      }
    });
    return newObjs;
  }

  function updateObjs(data) {
    data.forEach(function (d) {
      var obj = d[objBindAttr];

      if (obj) {
        obj[dataBindAttr] = d;
        updateObj(obj, d);
      }
    });
  }
}

export default viewDigest;
