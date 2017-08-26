import React, { PropTypes, Component} from 'react'
import ReactDOM from 'react-dom'
import App from './main'


window.onload = () => {
    ReactDOM.render(<App />, document.getElementById('main'))
}
