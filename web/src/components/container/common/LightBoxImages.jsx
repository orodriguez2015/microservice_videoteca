import React from 'react';
import ReactDOM from 'react-dom';
import Lightbox from "react-image-lightbox";
import 'react-image-lightbox/style.css'; 


/**
 * Component LightBox que muestra un LightBox de un conjunto de imágenes
 * que se le pasen a través de las propiedades del componente
 */
class LightBoxImages extends React.Component {

    constructor(props) {
        super(props);

        this.state = {
            photoIndex: 0,
            isOpen: this.props.isOpen,
            images:this.props.images
        };

        this.onCloseLightBox = this.onCloseLightBox.bind(this);
    }

    onCloseLightBox() {
        this.destroyComponent();
    }

    /**
     * Desmonta el componente que será invocado cuando se cierra el LightBox
     */
    destroyComponent() {
        var container = ReactDOM.findDOMNode(this).parentNode;
        ReactDOM.unmountComponentAtNode(container);
    }


    render() {
        const { photoIndex, isOpen } = this.state;
        console.log("LightBoxImages render isOpen: " + this.state.isOpen);
        console.log("LightBoxImages render photoIndex: " + JSON.stringify(this.state.photoIndex));

        return (
            <div>
            
                {(
                <Lightbox
                    mainSrc={this.state.images[photoIndex]}
                    nextSrc={this.state.images[(photoIndex + 1) % this.state.images.length]}
                    prevSrc={this.state.images[(photoIndex + this.state.images.length - 1) % this.state.images.length]}
                    onCloseRequest={() => this.onCloseLightBox()}
                    onMovePrevRequest={() =>
                    this.setState({
                        photoIndex: (photoIndex + this.state.images.length - 1) % this.state.images.length,
                    })
                    }
                    onMoveNextRequest={() =>
                    this.setState({
                        photoIndex: (photoIndex + 1) % this.state.images.length,
                    })
                    }
                />
                )}
            </div>
        );
    }

}

export default LightBoxImages;