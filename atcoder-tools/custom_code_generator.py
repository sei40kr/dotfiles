from typing import Any, Dict, Optional

from atcodertools.codegen.code_style_config import CodeStyleConfig
from atcodertools.codegen.models.code_gen_args import CodeGenArgs
from atcodertools.codegen.template_engine import render
from atcodertools.fmtprediction.models.format import (Format, ParallelPattern,
                                                      Pattern, SingularPattern,
                                                      TwoDimensionalPattern)
from atcodertools.fmtprediction.models.type import Type
from atcodertools.fmtprediction.models.variable import Variable


class RustCodeGenerator:
    def __init__(self,
                 format_: Optional[Format[Variable]],
                 config: CodeStyleConfig) -> None:
        self._format = format_
        self._config = config

    def generate_parameters(self) -> Dict[str, Any]:
        if self._format is None:
            return dict(prediction_success=False)

        return dict(formal_arguments=self._formal_arguments(),
                    actual_arguments=self._actual_arguments(),
                    input_part=self._input_part(),
                    prediction_success=True)

    def _input_part(self):
        lines = []
        lines.append('input! {')

        for pattern in self._format.sequence:
            var = pattern.all_vars()[0]

            if isinstance(pattern, SingularPattern):
                lines.append(self._generate_value_input(var))
            elif isinstance(pattern, ParallelPattern):
                lines.append(self._generate_array_input(var))
            elif isinstance(pattern, TwoDimensionalPattern):
                lines.append(self._generate_2darray_input(var))
            else:
                raise NotImplementedError

        lines.append('};')

        return "\n".join(lines)

    def _generate_value_input(self, var: Variable):
        return '{name}: {type},'.format(
            name=var.name,
            type=self._to_input_type(var.type))

    def _generate_array_input(self, var: Variable):
        return '{name}: [{type}; {num_rows} as usize],'.format(
            name=var.name,
            type=self._to_input_type(var.type),
            num_rows=var.first_index.get_length())

    def _generate_2darray_input(self, var: Variable):
        return '{name}: [[{type}; {num_cols} as usize]; {num_rows} as usize],'.format(
            name=var.name,
            type=self._to_input_type(var.type),
            num_cols=var.second_index.get_length(),
            num_rows=var.first_index.get_length())

    def _to_input_type(self, type_: Type):
        if type_ == Type.int:
            return 'i64'
        if type_ == Type.float:
            return 'f64'
        if type_ == Type.str:
            return 'chars'
        else:
            raise NotImplementedError

    def _actual_arguments(self) -> str:
        """
            :return always empty string
        """
        return ''

    def _formal_arguments(self) -> str:
        """
            :return always empty string
        """
        return ''


class NoPredictionResultGiven(Exception):
    pass


def main(args: CodeGenArgs) -> str:
    code_parameters = RustCodeGenerator(
        args.format, args.config).generate_parameters()
    return render(
        args.template,
        mod=args.constants.mod,
        yes_str=args.constants.yes_str,
        no_str=args.constants.no_str,
        **code_parameters
    )
