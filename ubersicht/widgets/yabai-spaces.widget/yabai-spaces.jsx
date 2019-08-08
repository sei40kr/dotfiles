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
    case 5:
      icon = '';
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
line-height: 30px;
position: absolute;
text-align: left;
top: 0;
width: 31vw;
z-index: 100;

.spaces {
  display: flex;
  align-items: flex-start;
  list-style: none;
  margin: 0;
  padding: 0;
}

.space {
  display: block;
  height: 30px;
  padding: 0 8px;

  &:not(:first-child) {
    margin-left: 8px;
  }

  &--focused {
    border-bottom: 2px solid #dfdfdf;
    color: #dfdfdf;
  }

  &__icon {
    font-size: 1.1em;
    font-family: 'Font Awesome 5 Free Solid';
  }
}
`;
