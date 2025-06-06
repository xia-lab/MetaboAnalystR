var index = (function (p) {
  return p instanceof Function ? p // fn
  : typeof p === 'string' ? function (obj) {
    return obj[p];
  } // property name
  : function (obj) {
    return p;
  };
}); // constant

export default index;
