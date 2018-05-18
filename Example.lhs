https://github.com/patrikja/SyntheticPopulations/blob/master/src/Draft.hs
https://github.com/patrikja/SyntheticPopulations/blob/master/src/Example1.hs

Example agents:
* green growth: household with (or without) a car
* health habits: individuals (age, gender, smoking, education, ...)

Need to describe
* which labels + types are in the agent
    * also need to describe the enumeration types
* which of those are used for IPF
    * those for which a small, but complete, sample is available




\begin{code}
ages          = ["Child", "Mid", "Old"]
incomelevels  = ["Poor", "Middle", "Rich"]
\end{code}

Perhaps use C++ SYNTAX: enum Suit { Diamonds, Hearts, Clubs, Spades };

enum Age { A10, A20, A30, A40, AgeMissing = -2 };


Format conversion?
Format checking? [There are usually special "error values" etc. say -2 for missing age information, etc.]
IPF algorithm generation?
