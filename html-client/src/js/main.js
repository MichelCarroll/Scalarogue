import React, { PropTypes, Component} from 'react'

module.exports = class App extends Component {
  
  constructor() {
    super();
    this.state = {};
  }
  
  componentDidMount() {
    const hooks = window.Main.start({
      seed: +(new Date()),
      viewport: document.getElementById('viewport'),
      minimap: document.getElementById('minimap'),
      onUpdateState: (state) => {
        console.log(state);
        this.setState(state)
      }
    });
    console.log(hooks);
    this.setState({ hooks })
  }
  
  renderStats() {
    if(!this.state.stats) {
      return null;
    }
    
    const renderBar = (current, max) => {
      const percentage = current / max * 100;
      return <div className="bar">
          <div className="inner" style={{background: 'red', width: percentage + "%"}}></div>
          <div className="annotation">
            {current+"/"+max}
          </div>
      </div>
    }
    
    const renderItem = (name, amount, slug) => <li onClick={() => this.state.hooks.useItem(slug)} key={slug}>
        <div className="name">{name}</div>
        <div className="value">{amount}</div>
    </li>
    
    return <div className="section stats-section">
        <ul id="stats-list">
            <li>
                <div className="name">Health</div>
                {renderBar(this.state.stats.health.current, this.state.stats.health.max)}
            </li>
            {this.state.items.map(({name, amount, slug}) => renderItem(name, amount, slug))}
        </ul>
    </div>
  }
  
  renderNotifications() {
    if(!this.state.notifications) {
      return null;
    }
    
    return <div id="message-container" className="message-container" width="600" height="200">
        <ul id="messages" className="messages">{
          this.state.notifications.map((notification, i) => <li key={i}>{notification}</li>)
        }</ul>
    </div>
  }
  
  render() {
    return <div>
      <canvas className="section" width="600" height="600" id="viewport"></canvas>
      <canvas className="section" width="200" height="200" id="minimap"></canvas>
      {this.renderStats()}
      {this.renderNotifications()}
    </div>
  }
}
