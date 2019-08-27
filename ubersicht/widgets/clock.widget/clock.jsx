export const command = "date '+%Y/%m/%d %H:%M'";

export const refreshFrequency = 1000;

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  return (
    <div className="clock">
      <span className="clock__icon"></span>
      <span className="clock__datetime">{output}</span>
    </div>
  );
};

export const className = `
color: #bfbfbf;
font-family: 'Input Mono', monospace;
font-size: 16px;
position: absolute;
right: 1vw;
text-align: right;
top: 0;
z-index: 100;

.clock {
  border-bottom: 3px solid #bfbfbf;
  height: 29px;
  line-height: 29px;
  padding: 0 0.5em;

  &__icon {
    -webkit-font-smoothing: antialiased;
    font-family: 'Font Awesome 5 Free Solid';
    font-size: 1.1em;
  }

  &__datetime {
    margin-left: 0.5em;
  }
}
`;
