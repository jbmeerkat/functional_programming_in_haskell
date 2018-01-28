integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b =
    let stepsNumber = 1000
        stepSize = (b - a) / stepsNumber
        initialValue = (f a + f b) / 2
    in integrationIter f (stepsNumber - 1) a stepSize initialValue

integrationIter :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double
integrationIter f 0 lowerBound stepSize value = value * stepSize
integrationIter f remainingSteps lowerBound stepSize value =
    let point = lowerBound + remainingSteps * stepSize
        valueAtPoint = f point
    in integrationIter f (remainingSteps - 1) lowerBound stepSize (value + valueAtPoint)
