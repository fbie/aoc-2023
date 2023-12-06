using System.Text;

record MaybePartNumber(int Start, int End, int Value, bool IsDefinitePartNumber);

class Line {
    private readonly HashSet<int> Symbols = new HashSet<int>();
    private readonly List<MaybePartNumber> PartNumbers = new List<MaybePartNumber>();

    public void AddSymbol(int i) => Symbols.Add(i);

    public void AddPartNumber(MaybePartNumber n) => PartNumbers.Add(n);

    public bool ContainsSymbol(int start, int end) {
        for (var i = start; i <= end; i++) {
            if (Symbols.Contains(i)) {
                return true;
            }
        }
        return false;
    }

    public List<MaybePartNumber> FindAdjacentPartNumbers(int i) {
        var ns = new List<MaybePartNumber>();
        foreach (var n in PartNumbers) {
            if (n.Start <= i && i <= n.End) {
                ns.Add(n);
            }
        }
        return ns;
    }
}

class MaybePartNumberBuilder {
    private readonly StringBuilder DigitBuilder = new StringBuilder();
    private readonly int StartIndex;
    private readonly bool IsDefinitePartNumber;

    public MaybePartNumberBuilder(int startIndex, bool isDefinitePartNumber) {
        StartIndex = Math.Max(0, startIndex - 1);
        IsDefinitePartNumber = isDefinitePartNumber;
    }

    public void Add(char c) {
        DigitBuilder.Append(c);
    }

    public MaybePartNumber ToMaybePartNumber(int index)
        => new MaybePartNumber(StartIndex, index, int.Parse(DigitBuilder.ToString()), IsDefinitePartNumber);
}

class State {
    private enum Kind { Empty, Symbol, Digit }
    private MaybePartNumberBuilder PartNumberBuilder;
    private Kind Last;

    public int Line { get; private set; }
    public int Column { get; private set; }

    public State() { Reset(); }

    public void Reset() {
        PartNumberBuilder = null;
        Last = Kind.Empty;
        Line = -1;
        Column++;
    }

    private MaybePartNumber MakeAndReset() {
        var n = PartNumberBuilder.ToMaybePartNumber(Line);
        PartNumberBuilder = null;
        return n;
    }

    public MaybePartNumber Symbol() {
        Last = Kind.Symbol;
        return PartNumberBuilder != null
            ? MakeAndReset() with { IsDefinitePartNumber = true }
            : null;
    }

    public void Digit(char c) {
        if (PartNumberBuilder == null) {
            PartNumberBuilder = new MaybePartNumberBuilder(Line, Last == Kind.Symbol);
        }
        PartNumberBuilder.Add(c);
        Last = Kind.Digit;
    }

    public MaybePartNumber Empty() {
        Last = Kind.Empty;
        return PartNumberBuilder != null
            ? MakeAndReset()
            : null;
    }

    public void Step() => Line++;
}

public static class GearRatios {
    static long SumParts(IEnumerable<char> schematic) {
        var state = new State();
        var previousLine = new Line();
        var currentLine = new Line();
        var gears = new Dictionary<(int, int), List<MaybePartNumber>>();
        void RecordMaybeGear(MaybePartNumber n) {
            var key = (state.Line, state.Column);
            if (gears.TryGetValue(key, out var ns)) {
                ns.Add(n);
            } else {
                ns = new List<MaybePartNumber> { n };
            }
        }
        var sum = 0L;
        foreach (var c in schematic) {
            void Add(MaybePartNumber n) {
                sum += n.Value;
            }

            void MaybeAdd(MaybePartNumber n) {
                if (n.IsDefinitePartNumber || previousLine.ContainsSymbol(n.Start, n.End)) {
                    Add(n);
                } else {
                    currentLine.AddPartNumber(n);
                }
            }

            void Empty() {
                if (state.Empty() is MaybePartNumber n) {
                    MaybeAdd(n);
                    if (c == '*') {
                        RecordMaybeGear(n);
                    }
                }
            }

            state.Step();

            switch (c) {
                case '\n': {
                    Empty();
                    previousLine = currentLine;
                    currentLine = new Line();
                    state.Reset();
                    continue;
                }
                case '.': {
                    Empty();
                    continue;
                }
                case char _ when Char.IsDigit(c): {
                    state.Digit(c);
                    continue;
                }
                default: {
                    currentLine.AddSymbol(state.Line);
                    var maybeGear = c == '*';
                    if (state.Symbol() is MaybePartNumber n) {
                        MaybeAdd(n);
                        if (maybeGear) {
                            RecordMaybeGear(n);
                        }
                    }
                    foreach (var m in previousLine.FindAdjacentPartNumbers(state.Line)) {
                        Add(m);
                        if (maybeGear) {
                            RecordMaybeGear(m);
                        }
                    }
                    continue;
                }
            }
        }
        return sum;
    }

    static IEnumerable<char> GetSchematic(string file) {
        using (var sr = new StreamReader(file)) {
            var buffer = new char[1024];
            var read = 0;
            while ((read = sr.ReadBlock(buffer, 0, buffer.Length)) > 0) {
                for (int i = 0; i < read; i++)
                    yield return buffer[i];
            }
        }
    }

    public static void Main(string[] args) {
        if (args.Length == 1) {
            Console.WriteLine(SumParts(GetSchematic(args[0])));
        }
    }
}
