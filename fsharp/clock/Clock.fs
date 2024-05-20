module Clock

type Clock =
    { Hours: int
      Minutes: int }
    member this.display() =
        sprintf "%s:%s" (this.Hours.ToString("00")) (this.Minutes.ToString("00"))

let [<Literal>] dayLength = 24 * 60

let calculate hours minutes =
    let totalMinutes = dayLength + (hours * 60 + minutes) % dayLength
    let hours = (totalMinutes / 60) % 24
    let minutes = totalMinutes % 60
    (hours, minutes)

let create hours minutes =
    let h, m = calculate hours minutes
    { Hours = h
      Minutes = m }

let add minutes clock =
    let h, m = calculate clock.Hours (minutes + clock.Minutes)
    { clock with
          Hours = h
          Minutes = m }

let subtract minutes clock =
    let h, m = calculate clock.Hours (clock.Minutes - minutes)
    { clock with
          Hours = h
          Minutes = m }

let display (clock: Clock) = clock.display()
