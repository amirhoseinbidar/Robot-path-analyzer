import math
import string
from enum import Enum
import matplotlib.pyplot as plt
import math


class Empty:
    pass


empty = Empty()
symbol_table = {
    "begin": empty,
    "end": empty,
    "east": 1,
    "west": -1,
    "north": 1,
    "south": -1,
    "up": 1,
    "down": -1,
}

space_bounds = [
    "lx",
    "ly",
    "ux",
    "uy",
    "uz",
    "lz",
    "ox",
    "oy",
    "oz",
]


class LexemeTypes(Enum):
    ASSIGN = ord(":")
    NUMBER = 256
    VARIABLE = 257
    KEY_WORD = 258
    END = 259


class Error(Exception):
    def __init__(self, message="") -> None:
        self.message = message
        super().__init__(message)

    def throw(self):
        print("\n" + self.__class__.__name__ + " : " + self.message + "\n")
        exit()


class InvalidCharacter(Error):
    def __init__(self, lexical_analyzer) -> None:
        line = lexical_analyzer.line_number + 1
        character_number = (
            lexical_analyzer.input_manager.forward - lexical_analyzer.last_line_start
        )
        message = f"Character '{lexical_analyzer.input_manager.get_char()}' is invalid in line {line}, number {character_number}"
        super().__init__(message)


class InvalidToken(Error):
    def __init__(self, guess, lexical_analyzer) -> None:
        line = lexical_analyzer.line_number + 1
        character_number = (
            lexical_analyzer.input_manager.forward - lexical_analyzer.last_line_start
        )
        message = f"could not recognize token in line {line}, number {character_number}, do you mean '{guess}' ?"
        super().__init__(message)


class InvalidSyntax(Error):
    def __init__(self, syntax_analyzer, description="") -> None:
        line = syntax_analyzer.analyzer.line_number + 1
        character_number = (
            syntax_analyzer.analyzer.input_manager.forward
            - syntax_analyzer.analyzer.last_line_start
        )
        message = f"invalid '{syntax_analyzer.look_ahead.value}' token in line {line}, number {character_number}\n{description}"
        super().__init__(message)


class InvalidSemantic(Error):
    pass


class InputFileManager:
    def __init__(self) -> None:
        self.input = open("input.txt", "r").read() + "\n"
        self.forward = -1
        self.__end = len(self.input)

    def next_char(self) -> str:
        self.forward += 1
        # function forward changed
        return self.input[self.forward]

    def retract(self) -> str:
        self.forward -= 1
        return self.input[self.forward]

    def get_char(self) -> str:
        return self.input[self.forward]

    def is_ended(self) -> bool:
        return self.forward + 1 >= self.__end


class Lexeme:
    def __init__(self, value, type) -> None:
        self.value = value
        self.type = type

    def __repr__(self) -> str:
        return f"Lexeme({self.value},{self.type})"


class LexicalAnalyzer:
    def __init__(self) -> None:
        self.input_manager = InputFileManager()
        self.lexeme_begin = 0
        self.line_number = 0
        self.last_line_start = 0

    def scan_variable(self) -> Lexeme:
        self.lexeme_begin = self.input_manager.forward
        while True:
            if (
                self.input_manager.next_char()
                not in string.ascii_letters + string.digits
            ):
                value = self.input_manager.input[
                    self.lexeme_begin : self.input_manager.forward
                ]
                self.input_manager.retract()

                if symbol_table.get(value.lower()):
                    return Lexeme(value, LexemeTypes.KEY_WORD)
                return Lexeme(value, LexemeTypes.VARIABLE)

    def scan_number(self) -> Lexeme:
        self.lexeme_begin = self.input_manager.forward
        while True:
            if self.input_manager.next_char() not in string.digits:
                value = self.input_manager.input[
                    self.lexeme_begin : self.input_manager.forward
                ]
                self.input_manager.retract()
                return Lexeme(value, LexemeTypes.NUMBER)

    def scan_one_comment(self) -> None:
        if self.input_manager.next_char() != "/":
            InvalidToken("//", self).throw()
        while True:
            if self.input_manager.next_char() == "\n":
                return self.input_manager.retract()

    def scan_multiple_comment(self) -> None:
        while True:
            if self.input_manager.next_char() == "}":
                return

    def get_token(self) -> Lexeme:
        while True:
            if self.input_manager.is_ended():
                return Lexeme(empty, LexemeTypes.END)

            char = self.input_manager.next_char()

            if char == ":":
                return Lexeme(char, LexemeTypes.ASSIGN)
            elif char == "/":
                self.scan_one_comment()
            elif char == "{":
                self.scan_multiple_comment()
            elif char in string.ascii_letters:
                return self.scan_variable()
            elif char in "+" + "-" + string.digits:
                return self.scan_number()
            elif char in string.whitespace:
                if char == "\n":
                    self.line_number += 1
                    self.last_line_start = self.input_manager.forward
            else:
                InvalidCharacter(self).throw()


class SyntaxAnalyzerBase:
    def __init__(self, lexical_analyzer=None, look_ahead=None):
        self.analyzer = lexical_analyzer
        if not lexical_analyzer:
            self.analyzer = LexicalAnalyzer()
        self.look_ahead = look_ahead

    def match(self, lex_type, value=None, raise_error=False):
        self.next()
        if self.look_ahead.type != lex_type:
            args = (
                self,
                f"Expected {lex_type.name } but got { self.look_ahead.type.name}",
            )
            if raise_error:
                raise InvalidSyntax(*args)
            else:
                InvalidSyntax(*args).throw()
        if value and self.look_ahead.value != value:
            args = (
                self,
                f"Expected {lex_type.name } but got { self.look_ahead.type.name}",
            )
            if raise_error:
                raise InvalidSyntax(*args)
            else:
                InvalidSyntax(*args).throw()

    def next(self):
        self.look_ahead = self.analyzer.get_token()

    def parse(self):
        # subclasses have to provide this method
        raise NotImplemented()


class RobotPathAnalyzer(SyntaxAnalyzerBase):
    def check_requirements(self):
        for bound in space_bounds:
            if not bound in symbol_table:
                InvalidSemantic(
                    f"'{bound}' variable is required for parsing robot path"
                ).throw()

        if not symbol_table["uz"] > symbol_table["lz"]:
            InvalidSemantic("uz have to be bigger than lz").throw()

        if not symbol_table["uy"] > symbol_table["ly"]:
            InvalidSemantic("uy have to be bigger than ly").throw()

        if not symbol_table["ux"] > symbol_table["lx"]:
            InvalidSemantic("ux have to be bigger than lx").throw()

        if not (symbol_table["lx"] <= symbol_table["ox"] <= symbol_table["ux"]):
            InvalidSemantic("ox is not in x bound").throw()
        if not (symbol_table["ly"] <= symbol_table["oy"] <= symbol_table["uy"]):
            InvalidSemantic("oy is not in y bound").throw()
        if not (symbol_table["lz"] <= symbol_table["oz"] <= symbol_table["uz"]):
            InvalidSemantic("oz is not in z bound").throw()

    def check_path(self, x, y, z):
        if (
            (x, y, z) in symbol_table.get("n", [])
            or not (symbol_table["lx"] <= x <= symbol_table["ux"])
            or not (symbol_table["ly"] <= y <= symbol_table["uy"])
            or not (symbol_table["lz"] <= z <= symbol_table["uz"])
        ):
            raise InvalidSyntax(self)

    def move(self, x_move, y_move, z_move):
        self.check_path(self.x + x_move, self.y + y_move, self.z + z_move)
        self.ax.quiver(self.x, self.y, self.z, x_move, y_move, z_move, color="blue")
        self.x += x_move
        self.y += y_move
        self.z += z_move
        print(f"({self.x}, {self.y}, {self.z})")

    def parse_movement(self):
        try:
            self.match(LexemeTypes.KEY_WORD, raise_error=True)
            if self.look_ahead.value.lower() == "end":
                print(f"L={self.count -1}")
                d = math.sqrt(
                    (self.x - symbol_table["ox"]) ** 2
                    + (self.y - symbol_table["oy"]) ** 2
                    + (self.z - symbol_table["oz"]) ** 2
                )
                print(f"D={d}")
                self.ax.scatter(self.x, self.y, self.z, color="black")
                self.ax.quiver(
                    symbol_table["ox"],
                    symbol_table["oy"],
                    symbol_table["oz"],
                    self.x - symbol_table["ox"],
                    self.y - symbol_table["oy"],
                    self.z - symbol_table["oz"],
                    color="green",
                )

                return

            elif self.look_ahead.value.lower() == "west":
                self.move(symbol_table["west"], 0, 0)

            elif self.look_ahead.value.lower() == "east":
                self.move(symbol_table["east"], 0, 0)

            elif self.look_ahead.value.lower() == "north":
                self.move(0, symbol_table["north"], 0)

            elif self.look_ahead.value.lower() == "south":
                self.move(0, symbol_table["south"], 0)

            elif self.look_ahead.value.lower() == "up":
                self.move(0, 0, symbol_table["up"])

            elif self.look_ahead.value.lower() == "down":
                self.move(0, 0, symbol_table["down"])

            self.count += 1
        except InvalidSyntax:
            print(f"Error in instr {self.count}")
        self.parse_movement()

    def parse(self):
        self.check_requirements()
        self.count = 1
        self.x = symbol_table["ox"]
        self.y = symbol_table["oy"]
        self.z = symbol_table["oz"]

        plt.figure()
        self.ax = plt.axes(projection="3d")
        self.ax.set_xlabel("X Axes")
        self.ax.set_ylabel("Y Axes")
        self.ax.set_zlabel("Z Axes")
        self.ax.view_init(10, 10)
        for obstacle_position in symbol_table.get("n", []):
            self.ax.scatter(*obstacle_position, color="red")
        plt.gcf().text(
            0.02,
            0.5,
            "",
            fontsize=8,
            bbox=dict(facecolor="red", alpha=0.5),
        )
        plt.gcf().text(
            0,
            0.84,
            (
                "* Blue arrows are the paths taken by the robot\n"
                "* Black point is last position of robot\n"
                "* Red points are obstacle\n"
                "* Green arrow is movement of robot"
            ),
            fontsize=10,
        )

        print(f"({self.x}, {self.y}, {self.z})")

        self.parse_movement()

        plt.show()


class SyntaxAnalyzer(SyntaxAnalyzerBase):
    def parse_list_variable(self, variable_lexeme):
        symbol_table[variable_lexeme.value.lower()] = []
        n = int(self.look_ahead.value)
        for _ in range(n):
            self.match(LexemeTypes.NUMBER)
            x = self.look_ahead
            self.match(LexemeTypes.NUMBER)
            y = self.look_ahead
            self.match(LexemeTypes.NUMBER)
            z = self.look_ahead
            symbol_table[variable_lexeme.value.lower()].append(
                (int(x.value), int(y.value), int(z.value))
            )

    def parse_single_variable(self, variable_lexeme):
        symbol_table[variable_lexeme.value.lower()] = int(self.look_ahead.value)

    def parse_variable(self):
        variable_lexeme = self.look_ahead
        self.match(LexemeTypes.ASSIGN)
        self.match(LexemeTypes.NUMBER)

        if variable_lexeme.value.lower() == "n":
            self.parse_list_variable(variable_lexeme)
        elif variable_lexeme.value.lower() in space_bounds:
            self.parse_single_variable(variable_lexeme)
        else:
            raise InvalidSyntax(self, "Invalid Variable")

    def parse(self):
        while True:
            self.next()
            if self.look_ahead.type == LexemeTypes.END:
                break
            if self.look_ahead.type == LexemeTypes.VARIABLE:
                self.parse_variable()
            if self.look_ahead.type == LexemeTypes.KEY_WORD:
                if self.look_ahead.value.lower() != "begin":
                    raise InvalidSyntax()
                RobotPathAnalyzer(self.analyzer, self.look_ahead).parse()
                return


SyntaxAnalyzer().parse()
