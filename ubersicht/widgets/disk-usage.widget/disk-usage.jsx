export const command = `df -m /|awk 'NR==2{print $2,$3}'`;

export const refreshFrequency = 60 * 1000;

const valueFormatter = new Intl.NumberFormat('en-US', {
  useGrouping: false,
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});

const humanize = (sizeInMegaBytes) => {
  let value = sizeInMegaBytes / 1024;
  let unit = 'GB';

  if (1024 <= value) {
    value /= 1024;
    unit = 'TB';
  }

  return valueFormatter.format(value) + unit;
};

const renderDiskUsage = (totalSize, usedSize) => {
  const diskUsage = usedSize / totalSize;
  const level = diskUsage < 0.25 ? 'low' : diskUsage < 0.75 ? 'medium' : 'high';

  return (
    <div className={`indicator indicator--${level}`}>
      <span className="indicator__icon"></span>
      <span className="indicator__value">
        {humanize(usedSize)}
        <span className="indicator__divider">/</span>
        {humanize(totalSize)}
      </span>
    </div>
  );
};

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const [totalSize, usedSize] = output.split(/\s+/);

  return renderDiskUsage(totalSize, usedSize);
};

export const className = `
-webkit-font-smoothing: subpixel-antialiased;
font-family: 'Input Mono', monospace;
font-size: 16px;
position: absolute;
right: calc(1vw + 644px + 1.5em);
text-align: right;
top: 0;
z-index: 100;

.indicator {
  border-bottom: 3px solid #fff;
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
