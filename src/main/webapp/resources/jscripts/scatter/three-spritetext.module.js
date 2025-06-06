import { LinearFilter, Sprite, SpriteMaterial, Texture } from "../three.module.js";

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

var three = typeof window !== 'undefined' && window.THREE ? window.THREE // Prefer consumption from global THREE, if exists
: {
  LinearFilter: LinearFilter,
  Sprite: Sprite,
  SpriteMaterial: SpriteMaterial,
  Texture: Texture
};

var _default = /*#__PURE__*/function (_three$Sprite) {
  _inherits(_default, _three$Sprite);

  var _super = _createSuper(_default);

  function _default() {
    var _this;

    var text = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    var textHeight = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;
    var color = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 'rgba(255, 255, 255, 1)';

    _classCallCheck(this, _default);

    _this = _super.call(this, new three.SpriteMaterial());
    _this._text = "".concat(text);
    _this._textHeight = textHeight;
    _this._color = color;
    _this._backgroundColor = false; // no background color

    _this._padding = 0;
    _this._borderWidth = 0;
    _this._borderRadius = 0;
    _this._borderColor = 'white';
    _this._strokeWidth = 0;
    _this._strokeColor = 'white';
    _this._fontFace = 'Arial';
    _this._fontSize = 90; // defines text resolution

    _this._fontWeight = 'normal';
    _this._canvas = document.createElement('canvas');

    _this._genCanvas();

    return _this;
  }

  _createClass(_default, [{
    key: "text",
    get: function get() {
      return this._text;
    },
    set: function set(text) {
      this._text = text;

      this._genCanvas();
    }
  }, {
    key: "textHeight",
    get: function get() {
      return this._textHeight;
    },
    set: function set(textHeight) {
      this._textHeight = textHeight;

      this._genCanvas();
    }
  }, {
    key: "color",
    get: function get() {
      return this._color;
    },
    set: function set(color) {
      this._color = color;

      this._genCanvas();
    }
  }, {
    key: "backgroundColor",
    get: function get() {
      return this._backgroundColor;
    },
    set: function set(color) {
      this._backgroundColor = color;

      this._genCanvas();
    }
  }, {
    key: "padding",
    get: function get() {
      return this._padding;
    },
    set: function set(padding) {
      this._padding = padding;

      this._genCanvas();
    }
  }, {
    key: "borderWidth",
    get: function get() {
      return this._borderWidth;
    },
    set: function set(borderWidth) {
      this._borderWidth = borderWidth;

      this._genCanvas();
    }
  }, {
    key: "borderRadius",
    get: function get() {
      return this._borderRadius;
    },
    set: function set(borderRadius) {
      this._borderRadius = borderRadius;

      this._genCanvas();
    }
  }, {
    key: "borderColor",
    get: function get() {
      return this._borderColor;
    },
    set: function set(borderColor) {
      this._borderColor = borderColor;

      this._genCanvas();
    }
  }, {
    key: "fontFace",
    get: function get() {
      return this._fontFace;
    },
    set: function set(fontFace) {
      this._fontFace = fontFace;

      this._genCanvas();
    }
  }, {
    key: "fontSize",
    get: function get() {
      return this._fontSize;
    },
    set: function set(fontSize) {
      this._fontSize = fontSize;

      this._genCanvas();
    }
  }, {
    key: "fontWeight",
    get: function get() {
      return this._fontWeight;
    },
    set: function set(fontWeight) {
      this._fontWeight = fontWeight;

      this._genCanvas();
    }
  }, {
    key: "strokeWidth",
    get: function get() {
      return this._strokeWidth;
    },
    set: function set(strokeWidth) {
      this._strokeWidth = strokeWidth;

      this._genCanvas();
    }
  }, {
    key: "strokeColor",
    get: function get() {
      return this._strokeColor;
    },
    set: function set(strokeColor) {
      this._strokeColor = strokeColor;

      this._genCanvas();
    }
  }, {
    key: "_genCanvas",
    value: function _genCanvas() {
      var _this2 = this;

      var canvas = this._canvas;
      var ctx = canvas.getContext('2d');
      var border = Array.isArray(this.borderWidth) ? this.borderWidth : [this.borderWidth, this.borderWidth]; // x,y border

      var relBorder = border.map(function (b) {
        return b * _this2.fontSize * 0.1;
      }); // border in canvas units

      var borderRadius = Array.isArray(this.borderRadius) ? this.borderRadius : [this.borderRadius, this.borderRadius, this.borderRadius, this.borderRadius]; // tl tr br bl corners

      var relBorderRadius = borderRadius.map(function (b) {
        return b * _this2.fontSize * 0.1;
      }); // border radius in canvas units

      var padding = Array.isArray(this.padding) ? this.padding : [this.padding, this.padding]; // x,y padding

      var relPadding = padding.map(function (p) {
        return p * _this2.fontSize * 0.1;
      }); // padding in canvas units

      var lines = this.text.split('\n');
      var font = "".concat(this.fontWeight, " ").concat(this.fontSize, "px ").concat(this.fontFace);
      ctx.font = font; // measure canvas with appropriate font

      var innerWidth = Math.max.apply(Math, _toConsumableArray(lines.map(function (line) {
        return ctx.measureText(line).width;
      })));
      var innerHeight = this.fontSize * lines.length;
      canvas.width = innerWidth + relBorder[0] * 2 + relPadding[0] * 2;
      canvas.height = innerHeight + relBorder[1] * 2 + relPadding[1] * 2; // paint border

      if (this.borderWidth) {
        ctx.strokeStyle = this.borderColor;

        if (relBorder[0]) {
          // left + right borders
          var hb = relBorder[0] / 2;
          ctx.lineWidth = relBorder[0];
          ctx.beginPath();
          ctx.moveTo(hb, relBorderRadius[0]);
          ctx.lineTo(hb, canvas.height - relBorderRadius[3]);
          ctx.moveTo(canvas.width - hb, relBorderRadius[1]);
          ctx.lineTo(canvas.width - hb, canvas.height - relBorderRadius[2]);
          ctx.stroke();
        }

        if (relBorder[1]) {
          // top + bottom borders
          var _hb = relBorder[1] / 2;

          ctx.lineWidth = relBorder[1];
          ctx.beginPath();
          ctx.moveTo(Math.max(relBorder[0], relBorderRadius[0]), _hb);
          ctx.lineTo(canvas.width - Math.max(relBorder[0], relBorderRadius[1]), _hb);
          ctx.moveTo(Math.max(relBorder[0], relBorderRadius[3]), canvas.height - _hb);
          ctx.lineTo(canvas.width - Math.max(relBorder[0], relBorderRadius[2]), canvas.height - _hb);
          ctx.stroke();
        }

        if (this.borderRadius) {
          // strike rounded corners
          var cornerWidth = Math.max.apply(Math, _toConsumableArray(relBorder));

          var _hb2 = cornerWidth / 2;

          ctx.lineWidth = cornerWidth;
          ctx.beginPath();
          [!!relBorderRadius[0] && [relBorderRadius[0], _hb2, _hb2, relBorderRadius[0]], !!relBorderRadius[1] && [canvas.width - relBorderRadius[1], canvas.width - _hb2, _hb2, relBorderRadius[1]], !!relBorderRadius[2] && [canvas.width - relBorderRadius[2], canvas.width - _hb2, canvas.height - _hb2, canvas.height - relBorderRadius[2]], !!relBorderRadius[3] && [relBorderRadius[3], _hb2, canvas.height - _hb2, canvas.height - relBorderRadius[3]]].filter(function (d) {
            return d;
          }).forEach(function (_ref) {
            var _ref2 = _slicedToArray(_ref, 4),
                x0 = _ref2[0],
                x1 = _ref2[1],
                y0 = _ref2[2],
                y1 = _ref2[3];

            ctx.moveTo(x0, y0);
            ctx.quadraticCurveTo(x1, y0, x1, y1);
          });
          ctx.stroke();
        }
      } // paint background


      if (this.backgroundColor) {
        ctx.fillStyle = this.backgroundColor;

        if (!this.borderRadius) {
          ctx.fillRect(relBorder[0], relBorder[1], canvas.width - relBorder[0] * 2, canvas.height - relBorder[1] * 2);
        } else {
          // fill with rounded corners
          ctx.beginPath();
          ctx.moveTo(relBorder[0], relBorderRadius[0]);
          [[relBorder[0], relBorderRadius[0], canvas.width - relBorderRadius[1], relBorder[1], relBorder[1], relBorder[1]], // t
          [canvas.width - relBorder[0], canvas.width - relBorder[0], canvas.width - relBorder[0], relBorder[1], relBorderRadius[1], canvas.height - relBorderRadius[2]], // r
          [canvas.width - relBorder[0], canvas.width - relBorderRadius[2], relBorderRadius[3], canvas.height - relBorder[1], canvas.height - relBorder[1], canvas.height - relBorder[1]], // b
          [relBorder[0], relBorder[0], relBorder[0], canvas.height - relBorder[1], canvas.height - relBorderRadius[3], relBorderRadius[0]] // t
          ].forEach(function (_ref3) {
            var _ref4 = _slicedToArray(_ref3, 6),
                x0 = _ref4[0],
                x1 = _ref4[1],
                x2 = _ref4[2],
                y0 = _ref4[3],
                y1 = _ref4[4],
                y2 = _ref4[5];

            ctx.quadraticCurveTo(x0, y0, x1, y1);
            ctx.lineTo(x2, y2);
          });
          ctx.closePath();
          ctx.fill();
        }
      }

      ctx.translate.apply(ctx, _toConsumableArray(relBorder));
      ctx.translate.apply(ctx, _toConsumableArray(relPadding)); // paint text

      ctx.font = font; // Set font again after canvas is resized, as context properties are reset

      ctx.fillStyle = this.color;
      ctx.textBaseline = 'bottom';
      var drawTextStroke = this.strokeWidth > 0;

      if (drawTextStroke) {
        ctx.lineWidth = this.strokeWidth * this.fontSize / 10;
        ctx.strokeStyle = this.strokeColor;
      }

      lines.forEach(function (line, index) {
        var lineX = (innerWidth - ctx.measureText(line).width) / 2;
        var lineY = (index + 1) * _this2.fontSize;
        drawTextStroke && ctx.strokeText(line, lineX, lineY);
        ctx.fillText(line, lineX, lineY);
      }); // Inject canvas into sprite

      if (this.material.map) this.material.map.dispose(); // gc previous texture

      var texture = this.material.map = new three.Texture(canvas);
      texture.minFilter = three.LinearFilter;
      texture.needsUpdate = true;
      var yScale = this.textHeight * lines.length + border[1] * 2 + padding[1] * 2;
      this.scale.set(yScale * canvas.width / canvas.height, yScale, 0);
    }
  }, {
    key: "clone",
    value: function clone() {
      return new this.constructor(this.text, this.textHeight, this.color).copy(this);
    }
  }, {
    key: "copy",
    value: function copy(source) {
      three.Sprite.prototype.copy.call(this, source);
      this.color = source.color;
      this.backgroundColor = source.backgroundColor;
      this.padding = source.padding;
      this.borderWidth = source.borderWidth;
      this.borderColor = source.borderColor;
      this.fontFace = source.fontFace;
      this.fontSize = source.fontSize;
      this.fontWeight = source.fontWeight;
      this.strokeWidth = source.strokeWidth;
      this.strokeColor = source.strokeColor;
      return this;
    }
  }]);

  return _default;
}(three.Sprite);

export { _default as default };
