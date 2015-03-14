(function(global) {

function getRandomNumber() {
  return Math.random()
}

// Return an Array containing n random numbers and no holes.
function getRandomArray(n) {
  var r = []
  for (var i = 0; i < n; i++) {
    r.push(getRandomNumber());
  }
  return r
}

function runBenchmark(n) {
  var suite = new Benchmark.Suite()
  var array = getRandomArray(n)
  var item  = getRandomNumber()

  suite.add('Array#concat', function() {
    [item].concat(array)
  })
  suite.run()
  return suite[0].stats
}

function rangeUpTo(n) {
  var r = []
  for (var i = 1; i <= n; i++) {
    r.push(i)
  }
  return r
}

function go() {
  var result = null
  return rangeUpTo(50)
    .map(function(n) { return 2000 * n })
    .map(function(n) { return runBenchmark(n) })
}

global.Main = {
  'getRandomNumber': getRandomNumber,
  'getRandomArray': getRandomArray,
  'runBenchmark': runBenchmark,
  'rangeUpTo': rangeUpTo,
  'go': go,
};

})(this)
