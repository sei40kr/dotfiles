export const command = './outdated-packages.widget/libexec/outdated-packages';

export const refreshFrequency = 60 * 60 * 1000;

const valueFormatter = new Intl.NumberFormat('en-US');

const renderOutdatedFormulas = (value) => {
  const upToDate = value == 0;

  return (
    <div
      className={`indicator ${
        !upToDate ? 'indicator--outdated' : 'indicator--up-to-date'
      }`}
    >
      <span className="indicator__icon">{!upToDate ? '' : ''}</span>
      <span className="indicator__value">
        {!upToDate ? valueFormatter.format(value) : 'up-to-date'}
      </span>
    </div>
  );
};

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const value = Number(output);

  return renderOutdatedFormulas(value);
};

export const className = `
-webkit-font-smoothing: subpixel-antialiased;
font-family: 'Input Mono', monospace;
font-size: 16px;
position: absolute;
right: calc(1vw + 206px + 0.5em);
text-align: right;
top: 0;
z-index: 100;

.indicator {
  border-bottom: 3px solid transparent;
  height: 29px;
  line-height: 29px;
  padding: 0 0.5em;

  &--up-to-date {
    border-color: #98be65;
    color: #98be65;
  }

  &--outdated {
    border-color: #da8548;
    color: #da8548;
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
