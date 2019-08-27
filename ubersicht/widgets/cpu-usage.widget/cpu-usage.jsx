export const command = `iostat -dCn 0 | awk '2<NR{print $1+$2+$3}'`;

export const refreshFrequency = 1000;

const valueFormatter = new Intl.NumberFormat('en-US', {
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});

const renderCpuUsage = (cpuUsage) => {
  const level = cpuUsage < 30 ? 'low' : cpuUsage < 80 ? 'medium' : 'high';

  return (
    <div key="cpu" className={`indicator indicator--${level}`}>
      <span className="indicator__icon"></span>
      <span className="indicator__value">
        {valueFormatter.format(cpuUsage)}%
      </span>
    </div>
  );
};

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const cpuUsage = Number(output);

  return renderCpuUsage(cpuUsage);
};

export const className = `
-webkit-font-smoothing: subpixel-antialiased;
display: flex;
font-family: 'Input Mono', monospace;
font-size: 16px;
position: absolute;
right: calc(1vw + 1058px + 2.5em);
text-align: right;
top: 0;
z-index: 100;

.indicator {
  border-bottom: 3px solid transparent;
  height: 29px;
  line-height: 29px;
  padding: 0 0.5em;

  &--low {
    border-color: #98be65;
    color: #98be65;
  }

  &--medium {
    border-color: #ecbe7b;
    color: #ecbe7b;
  }

  &--high {
    border-color: #ff6c6b;
    color: #ff6c6b;
  }

  &__icon {
    -webkit-font-smoothing: antialiased;
    font-family: 'Font Awesome 5 Free Solid';
    font-size: 1.1em;
  }

  &__value {
    margin-left: 0.5em;
  }
}
`;
