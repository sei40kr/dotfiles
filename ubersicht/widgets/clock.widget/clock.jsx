export const command = "date '+%Y/%m/%d %H:%M'";

export const refreshFrequency = 1000;

export const render = ({ output }) => {
    if (!output) {
        return null;
    }

    return output;
};

export const className = `
color: #bfbfbf;
font-family: Menlo, monospace;
font-size: 16px;
height: 32px;
line-height: 32px;
position: absolute;
right: 1vw;
text-align: right;
top: 0;
width: 31vw;
z-index: 100;
`;
