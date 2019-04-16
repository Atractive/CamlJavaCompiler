/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author mambrois
 */
public class PairV extends Value{
    private Value fst, snd;
    
    public PairV(Value fst, Value snd){
        this.fst = fst;
        this.snd = snd;
    }
    
    public Value getFstValue(){return fst;}
    public Value getSndValue(){return snd;}
     
    public void setFstValue(Value nFst){fst = nFst;}
    public void setSndValue(Value nSnd){snd = nSnd;}
        
}
