import sys


class Interpreter:
    def __init__(self):
        self.memory = {"perm": [], "temp": []}
        self.instructions = []
        self.labels = {}
        self.pc = 0
        self.stack = []

    def _getmem(self, typ, key):
        mem = self.memory[typ]
        if key >= len(mem):
            mem.extend([0] * (key+1-len(mem)))
        return mem[key]

    def _setmem(self, typ, key, val):
        mem = self.memory[typ]
        if key >= len(mem):
            mem.extend([0] * (key+1-len(mem)))
        mem[key] = val

    def get(self, key):
        if key[0] == "a":
            return self._getmem("perm", int(key[1:]))
        elif key[0] == "t":
            return self._getmem("temp", int(key[1:]))
        else:
            raise Exception(f"bad key: {key}")

    def set(self, key, val):
        if key[0] == "a":
            self._setmem("perm", int(key[1:]), val)
        elif key[0] == "t":
            self._setmem("temp", int(key[1:]), val)
        else:
            raise Exception(f"bad key: {key}")

    def load(self, file):
        self.instructions.clear()
        self.labels.clear()
        self.pc = 0
        with open(file) as f:
            for line in f:
                if line := line.strip():
                    self.instructions.append(line.split())
        self.scanlabels()

    def scanlabels(self):
        for i, line in enumerate(self.instructions):
            if line[0][-1] == ":":
                self.labels[line[0][:-1]] = i

    def interpretValue(self, val):
        if val.startswith("0x"):
            return int(val, 16)
        else:
            return self.get(val)

    def execute(self):
        cmd = self.instructions[self.pc]
        # print(cmd)
        self.pc += 1
        if cmd[0] == "MOV":
            self.set(cmd[1], self.interpretValue(cmd[2]))
        elif cmd[0] == "JNZ":
            v = self.interpretValue(cmd[1])
            if v != 0:
                self.pc = self.labels[cmd[2]]
        elif cmd[0] == "NOT":
            v = self.interpretValue(cmd[2])
            self.set(cmd[1], 0 if v else 1)
        elif cmd[0] == "ODD":
            v = self.interpretValue(cmd[2])
            self.set(cmd[1], v % 2)
        elif cmd[0].endswith(":"):
            pass
        elif cmd[0] == "PRINT":
            v = self.interpretValue(cmd[1])
            print(v)
        elif cmd[0] == "READ":
            val = input("enter number: ")
            self.set(cmd[1], int(val))
        elif cmd[0] == "CALL":
            pos = self.labels[cmd[1]]
            self.stack.append(self.pc)
            self.pc = pos
        elif cmd[0] == "RET":
            self.pc = self.stack.pop()
        elif cmd[0] == "ADD":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], lhs + rhs)
        elif cmd[0] == "SUB":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], lhs - rhs)
        elif cmd[0] == "MUL":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], lhs * rhs)
        elif cmd[0] == "DIV":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], lhs // rhs)
        elif cmd[0] == "LT":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], 1 if lhs < rhs else 0)
        elif cmd[0] == "LTE":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], 1 if lhs <= rhs else 0)
        elif cmd[0] == "GT":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], 1 if lhs > rhs else 0)
        elif cmd[0] == "GTE":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], 1 if lhs >= rhs else 0)
        elif cmd[0] == "EQ":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], 1 if lhs == rhs else 0)
        elif cmd[0] == "NE":
            lhs = self.interpretValue(cmd[2])
            rhs = self.interpretValue(cmd[3])
            self.set(cmd[1], 1 if lhs != rhs else 0)
        else:
            raise Exception(f"unknown operation: {cmd[0]}")

    def run(self):
        self.pc = 0
        while self.pc < len(self.instructions):
            self.execute()


if __name__ == "__main__":
    file = sys.argv[1]
    interpreter = Interpreter()
    interpreter.load(file)
    interpreter.run()
