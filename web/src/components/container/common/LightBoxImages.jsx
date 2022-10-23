import React from 'react';
import ReactDOM from 'react-dom';
import {AlbumFacade} from '../../../facade/AlbumFacade';
import { AlmacenFacade } from '../../../store/AlmacenFacade';
import {ESTADO_PUBLICACION_FOTO,ESTADO_DESPUBLICACION_FOTO} from '../../../constantes/Constantes';
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
     * Se envia petición al servidor
     * @param {Integer} id 
     */
     onCallbackImageLoad(id) {
        console.log("id = " + id);
        
        if(this.props.activatePhotoDisplayCount===true) {
          

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
                    mainSrc={images[photoIndex].url}
                    nextSrc={images[(photoIndex + 1) % images.length].url}
                    prevSrc={images[(photoIndex + images.length - 1) % images.length].url}
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
                        (imageSrc,srcType,image)=> this.onCallbackImageLoad(images[photoIndex].id)
                    }
                   
                   
                />
                )}
            </div>
        );
    }

}

export default LightBoxImages;