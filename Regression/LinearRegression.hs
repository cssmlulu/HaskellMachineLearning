import MyMatrix as M


-- input xtrain and ytrain to learn the paramter theta
-- Normal Equation method: theta = (X^TX)^-1X^Ty 
linearRegressionNETrain :: M.Matrix -> M.Matrix -> M.Matrix
linearRegressionNETrain x y = theta
    where
        theta = xhat M.<> y
        xhat = (M.inv (xt M.<> x)) M.<> xt
        xt = M.trans xli    