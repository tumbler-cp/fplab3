module Test

open NUnit.Framework
open InterpolationAlgos

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test Linear Interpolation`` () =
    let p0 = { X = 1.0; Y = 2.0 }
    let p1 = { X = 3.0; Y = 4.0 }
    let x = 2.0
    let expected = 3.0
    let result = linearInterpolate p0 p1 x
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Test Newton Interpolation`` () =
    let points = [{ X = 1.0; Y = 1.0 }; { X = 2.0; Y = 4.0 }; { X = 3.0; Y = 9.0 }]
    let x = 2.5
    let expected = 6.25
    let result = newtonInterpolate points x
    Assert.That(result, Is.EqualTo(expected).Within 1e-6)

[<Test>]
let ``Test Lagrange Interpolation`` () =
    let points = [{ X = 1.0; Y = 1.0 }; { X = 2.0; Y = 4.0 }; { X = 3.0; Y = 9.0 }]
    let x = 2.5
    let expected = 6.25
    let result = lagrangeInterpolate points x
    Assert.That(result, Is.EqualTo(expected).Within 1e-6)

[<Test>]
let ``Test Parse Line`` () =
    let line = "1.0 2.0"
    let expected = Some { X = 1.0; Y = 2.0 }
    let result = parseLine line
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Test Parse Line with Invalid Data`` () =
    let line = "invalid data"
    let expected = None
    let result = parseLine line
    Assert.That(result, Is.EqualTo expected)