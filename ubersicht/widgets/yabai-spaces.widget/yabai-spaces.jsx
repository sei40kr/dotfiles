export const command = '/usr/local/bin/yabai -m query --spaces';

export const refreshFrequency = false;

const renderSpaces = (spaces) => (
  <ol className="spaces">{spaces.map(renderSpace)}</ol>
);

const renderSpace = (space) => {
  const isFocused = space.focused === 1;

  let icon;
  switch (space.index) {
    case 1:
      icon = '';
      break;
    case 2:
      icon = '';
      break;
    case 3:
      icon = '';
      break;
    case 4:
      icon = '';
      break;
    default:
      icon = '';
  }

  return (
    <li
      key={space.id}
      value={space.index}
      className={`space ${isFocused ? 'space--focused' : ''}`}
    >
      <span className="space__icon">{icon}</span>
    </li>
  );
};

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const spaces = JSON.parse(output);

  return renderSpaces(spaces);
};

export const className = `
color: #bfbfbf;
font-size: 16px;
left: 1vw;
position: absolute;
top: 0;
z-index: 100;

.spaces {
  display: flex;
  list-style: none;
  margin: 0;
  padding: 0;
}

.space {
  border-bottom: 3px solid transparent;
  height: 29px;
  line-height: 29px;
  padding: 0 0.5em;

  &:not(:last-child) {
    margin-right: 0.5em;
  }

  &--focused {
    border-color: #51afef;
    color: #51afef;
  }

  &__icon {
    -webkit-font-smoothing: antialiased;
    font-size: 1.1em;
    font-family: 'Font Awesome 5 Free Solid';
  }
}
`;
