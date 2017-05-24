/*
 * Copyright 2017 Rui Vieira
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.ruivieira.oresme.timeseries

object DoubleExponentialSmoothing {

  def calculate(data: List[Double],
                alpha: Double,
                gamma: Double): (List[Double], List[Double]) = {

    val n = data.size

    val s = new Array[Double](n)
    val b = new Array[Double](n)

    s(0) = data.head
    b(0) = data(1) - data.head

    (1 until n).foreach { t =>
      s(t) = alpha * data(t) + (1.0 - alpha) * (s(t - 1) + b(t - 1))

      b(t) = gamma * (s(t) - s(t - 1)) + (1.0 - gamma) * b(t - 1)

    }

    (s.toList, b.toList)
  }

  def forecast(steps: Int,
               alpha: Double,
               gamma: Double,
               s0: Double,
               b0: Double): List[Double] = {

    val f = new Array[Double](steps)

    var st = s0
    var bt = b0

    (0 until steps).foreach { t =>
      f(t) = st + bt

      val st1 = st

      st = alpha * f(t) + (1.0 - alpha) * (st + bt)
      bt = gamma * (st - st1) + (1.0 - gamma) * bt

    }

    f.toList

  }

}
