// Truncating division
function tdiv(x,y) {
  return x / y | 0;
}

function tmod(x,y) {
  return x % y;
}

// Flooring/Knuthian division
function fdiv(x,y) {
  return Math.floor(x/y);
}

function fmod(x,y) {
  return ((x % y) + y) % y;
}

// Euclidean division
function ediv(x,y) {
  return Math.sign(y) * Math.floor(x / Math.abs(y));
}

function emod(x,y) {
  var yy = Math.abs(y);
  return ((x % yy) + yy) % yy;
}

// Check that all of these satisfy the laws
function test(name, div, mod) {
  function err(name, msg) {
    throw new Error("While testing " + name + "division: " + msg);
  }

  var cases = [[2,3], [-2,3], [2,-3], [-2, -3]];
  for (var i = 0; i < cases.length; i++) {
    var a = cases[i][0];
    var b = cases[i][1];

    var q = div(a,b);
    var r = mod(a,b);

    if (b * q + r !== a) {
      err(name, "constraint 1 was not satisfied" 
        + " for a: " + a + ", b: " + b);
    }

    if (Math.abs(q - (a/b)) >= 1) {
      err(name, "constraint 2 was not satisfied" 
        + " for a: " + a + ", b: " + b);
    }

    if (name === "Euclidean") {
      if (r < 0 || r >= Math.abs(b)) {
        err(name, "Bad remainder"
          + " for a: " + a + ", b: " + b);
      }
    }
  }

  console.log("Tests passed for " + name + " division");
}

test("Truncating", tdiv, tmod);
test("Flooring", fdiv, fmod);
test("Euclidean", ediv, emod);

function gcd(a,b) {
  var aa = Math.abs(a);
  var bb = Math.abs(b);
  var go = function(x,y) {
    if (y === 0) {
      return x;
    } else {
      return gcd(y, emod(x,y));
    }
  }
  return go(aa,bb);
}

function reduceFraction(a,b) {
  var d = gcd(a,b);
  var numerator = a/d;
  var denominator = b/d;
  if (denominator < 0) {
    numerator *= -1;
    denominator *= -1;
  }
  return [numerator, denominator];
}

function primeFactors(m) {
  var n = m < 2 ? 1 : Math.abs(m);
  var nextDivisor = function(d) {
    return d === 2 ? 3 : d+2;
  }
  var factors = [];
  var divisor = 2;

  while (n !== 1) {
    if (emod(n, divisor) === 0) {
      factors.push(divisor);
      n = ediv(n, divisor);
    } else {
      divisor = nextDivisor(divisor);
    }
  }

  return factors;
}

function hasFiniteDecimalRepr(frac) {
  var is2or5 = function(p) { return p === 2 || p === 5 };
  return primeFactors(frac[1]).every(is2or5);
}

function exactRepr(a,b) {
  var frac = reduceFraction(a,b);
  if (hasFiniteDecimalRepr(frac)) {
    return String(frac[0]) + "/" + String(frac[1]) +
      " = " + String(a/b);
  } else {
    return String(frac[0]) + "/" + String(frac[1]) +
      " ≈ " + (a/b).toFixed(3) + "...";
  }
}

var elDividend = document.getElementById("input-dividend");
var elDivisor = document.getElementById("input-divisor");
var elExactResult = document.getElementById("span-exact-result");
var elTruncResult = document.getElementById("span-trunc-result");
var elFloorResult = document.getElementById("span-floor-result");
var elEuclideanResult = document.getElementById("span-euclidean-result");

function refreshTable() {
  var a = parseInt(elDividend.value);
  var b = parseInt(elDivisor.value);
  if (!isNaN(a) && !isNaN(b)) {
    elExactResult.innerText = exactRepr(a,b);
    elTruncResult.innerText = "q = " + tdiv(a,b) + ", r = " + tmod(a,b);
    elFloorResult.innerText = "q = " + fdiv(a,b) + ", r = " + fmod(a,b);
    elEuclideanResult.innerText = "q = " + ediv(a,b) + ", r = " + emod(a,b);
  }
}

elDividend.value = -10;
elDivisor.value = 3;
refreshTable();

elDividend.addEventListener('change', refreshTable);
elDivisor.addEventListener('change', refreshTable);
