export const command = `./ram-usage.widget/libexec/ram-usage`;

export const refreshFrequency = 1000;

const valueFormatter = new Intl.NumberFormat('en-US', {
  useGrouping: false,
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});

const humanize = (sizeInBytes) => {
  let value = sizeInBytes / 1024 / 1024 / 1024;
  let unit = 'GB';

  if (1024 <= value) {
    value /= 1024;
    unit = 'TB';
  }

  return valueFormatter.format(value) + unit;
};

const renderRamUsage = (usedRam, freeRam) => {
  const ramUsage = usedRam / (usedRam + freeRam);
  const level = ramUsage < 0.3 ? 'low' : ramUsage < 0.8 ? 'medium' : 'high';

  return (
    <div className={`indicator indicator--${level}`}>
      <span className="indicator__icon"></span>
      <span className="indicator__value">
        {humanize(usedRam)}
        <span className="indicator__divider">/</span>
        {humanize(usedRam + freeRam)}
      </span>
    </div>
  );
};

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const [rawUsedRam, rawFreeRam] = output.split(/\s+/);
  const usedRam = Number(rawUsedRam);
  const freeRam = Number(rawFreeRam);

  return renderRamUsage(usedRam, freeRam);
};

export const className = `
-webkit-font-smoothing: subpixel-antialiased;
display: flex;
font-family: 'Input Mono', monospace;
font-size: 16px;
position: absolute;
right: calc(1vw + 869px + 2em);
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

  &__divider {
    color: #9ca0a4;
    margin: 0 0.25em;
  }
}
`;
