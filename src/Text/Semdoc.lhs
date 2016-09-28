\begin{code}
module Text.Semdoc (module X) where

import           Text.Semdoc.IO        as X (phaseRead, phaseWrite)
import           Text.Semdoc.Processor as X (phaseEval)

\end{code}

\show{Str $ show $ 2 + 3}

Hello there.

\begin{lstlisting}[semdoc]
it = Table [] [AlignLeft, AlignLeft] [0.0, 0.0]
  [[Plain [Str "Byte", Space, Str "value"]],
   [Plain [Str "Packet", Space, Str "Kind"]]]
  [[[Plain [Code ("", [], []) "0x00"]],
   [Plain [Str "Ping", Space, Str "Request"]]]]
\end{lstlisting}
