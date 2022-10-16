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
            photoIndex: this.props.photoIndex,
            images:this.props.images
        };

        this.onCloseLightBox = this.onCloseLightBox.bind(this);
        this.onCallbackImageLoad = this.onCallbackImageLoad.bind(this);
    }


    /**
     * Callback que 
     * @param {int} photoIndex 
     */
    onCallbackImageLoad(index) {
        if(typeof(this.props.callbackOnImageLoad)==="function") {
            this.props.callbackOnImageLoad(index);
        }
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
        const { photoIndex, images } = this.state;
        
        return (
            <div>
            
                {(
                <Lightbox
                    mainSrc={images[photoIndex]}
                    nextSrc={images[(photoIndex + 1) % images.length]}
                    prevSrc={images[(photoIndex + images.length - 1) % images.length]}
                    onCloseRequest={() => this.onCloseLightBox()}
                    onMovePrevRequest={() =>
                    this.setState({
                        photoIndex: (photoIndex + images.length - 1) % images.length,
                    })
                    }
                    onMoveNextRequest={() =>
                    this.setState({
                        photoIndex: (photoIndex + 1) % images.length,
                    })
                    }

                    onImageLoad={
                        (imageSrc,srcType,image)=> this.onCallbackImageLoad(photoIndex)
                    }
                   
                   
                />
                )}
            </div>
        );
    }

}

export default LightBoxImages;