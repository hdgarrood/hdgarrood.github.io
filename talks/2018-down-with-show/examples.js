

// are promises ref. trans.?
function logSomething(msg) {
  return new Promise(function(resolve, reject) {
    console.log(msg);
    resolve();
  });
}

var p = logSomething("hi");

p.then(function() { return p });
