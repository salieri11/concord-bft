import click
from utils.text_edit import Color


class ColorPrint:

    @staticmethod
    def print_fail(cid: str, state: str) -> None:
        click.secho(
            f"{Color.BLUE}Correlation ID: {Color.PURPLE}{cid} {Color.WHITE}"
            f" failed to find the following state: {Color.GREEN}'{state}'")

    @staticmethod
    def print_duration(cid: str, duration: str, thld: str) -> None:
        click.secho(
            f"{Color.BLUE}Correlation ID: {Color.PURPLE}{cid} {Color.WHITE}submitted, duration "
            f"{Color.GREEN}{duration}ms {Color.WHITE}passed the threshold {Color.GREEN}{thld}ms")
