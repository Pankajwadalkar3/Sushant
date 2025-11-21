import React, { useCallback } from 'react';
import { Upload, FileCode, ArrowRight, Settings } from 'lucide-react';
import { TransformationTarget } from '../types';

interface UploadSectionProps {
    onUpload: (files: FileList) => void;
    target: TransformationTarget;
    setTarget: (t: TransformationTarget) => void;
    isProcessing: boolean;
}

const UploadSection: React.FC<UploadSectionProps> = ({ onUpload, target, setTarget, isProcessing }) => {
    const handleDrop = useCallback((e: React.DragEvent) => {
        e.preventDefault();
        if (e.dataTransfer.files && e.dataTransfer.files.length > 0) {
            onUpload(e.dataTransfer.files);
        }
    }, [onUpload]);

    const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        if (e.target.files && e.target.files.length > 0) {
            onUpload(e.target.files);
        }
    };

    return (
        <div className="max-w-3xl mx-auto space-y-8 animate-fade-in">
            <div className="text-center space-y-4">
                <h1 className="text-4xl font-bold text-slate-900 tracking-tight">
                    Legacy<span className="text-indigo-600">Forge</span> AI
                </h1>
                <p className="text-lg text-slate-600">
                    Transform mainframe logic to modern cloud-native architectures.
                </p>
            </div>

            {/* Configuration Card */}
            <div className="bg-white rounded-xl shadow-sm border border-slate-200 p-6 transition-all hover:shadow-md">
                <div className="flex items-center gap-2 mb-4 text-slate-800 font-semibold">
                    <Settings className="w-5 h-5 text-indigo-600" />
                    <h2>Configuration</h2>
                </div>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div>
                        <label className="block text-sm font-medium text-slate-700 mb-2">Target Technology</label>
                        <div className="flex gap-3">
                            <button
                                onClick={() => setTarget(TransformationTarget.DOTNET)}
                                className={`flex-1 py-3 px-4 rounded-lg border text-sm font-medium transition-all ${
                                    target === TransformationTarget.DOTNET
                                        ? 'bg-indigo-50 border-indigo-600 text-indigo-700 shadow-sm'
                                        : 'bg-white border-slate-200 text-slate-600 hover:border-slate-300'
                                }`}
                            >
                                .NET 8 / C#
                            </button>
                            <button
                                onClick={() => setTarget(TransformationTarget.JAVA)}
                                className={`flex-1 py-3 px-4 rounded-lg border text-sm font-medium transition-all ${
                                    target === TransformationTarget.JAVA
                                        ? 'bg-orange-50 border-orange-600 text-orange-700 shadow-sm'
                                        : 'bg-white border-slate-200 text-slate-600 hover:border-slate-300'
                                }`}
                            >
                                Java / Spring
                            </button>
                        </div>
                    </div>
                    <div>
                        <label className="block text-sm font-medium text-slate-700 mb-2">Transformation Mode</label>
                        <select className="w-full border-slate-200 rounded-lg text-sm py-3 px-4 focus:ring-indigo-500 focus:border-indigo-500">
                            <option>Clean Architecture (Recommended)</option>
                            <option>Vertical Slice Architecture</option>
                            <option>Microservices (Domain Driven)</option>
                        </select>
                    </div>
                </div>
            </div>

            {/* Upload Area */}
            <div 
                onDrop={handleDrop}
                onDragOver={(e) => e.preventDefault()}
                className="group relative border-2 border-dashed border-slate-300 rounded-xl p-12 text-center hover:border-indigo-500 hover:bg-indigo-50/50 transition-all cursor-pointer"
            >
                <input 
                    type="file" 
                    className="absolute inset-0 w-full h-full opacity-0 cursor-pointer z-10"
                    onChange={handleFileChange}
                    accept=".cbl,.cob,.cpy,.zip" 
                />
                <div className="space-y-4 pointer-events-none">
                    <div className="w-16 h-16 mx-auto bg-indigo-100 rounded-full flex items-center justify-center group-hover:scale-110 transition-transform duration-300">
                        {isProcessing ? (
                            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-indigo-600"></div>
                        ) : (
                            <Upload className="w-8 h-8 text-indigo-600" />
                        )}
                    </div>
                    <div>
                        <p className="text-lg font-medium text-slate-900">
                            {isProcessing ? 'Analyzing legacy code...' : 'Drop COBOL zip file here'}
                        </p>
                        <p className="text-sm text-slate-500 mt-1">
                            Supports .cbl, .cob, .cpy sources
                        </p>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-center text-sm text-slate-500">
                <div className="flex flex-col items-center p-4 bg-slate-50 rounded-lg">
                    <FileCode className="w-6 h-6 mb-2 text-slate-400" />
                    <span>Intelligent Parsing</span>
                </div>
                <div className="flex flex-col items-center p-4 bg-slate-50 rounded-lg">
                    <Settings className="w-6 h-6 mb-2 text-slate-400" />
                    <span>Dependency Mapping</span>
                </div>
                <div className="flex flex-col items-center p-4 bg-slate-50 rounded-lg">
                    <ArrowRight className="w-6 h-6 mb-2 text-slate-400" />
                    <span>Modern Architecture</span>
                </div>
            </div>
        </div>
    );
};

export default UploadSection;